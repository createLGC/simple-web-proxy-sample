#include <iostream>
#include <thread>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <unistd.h>

#include "http.hpp"
#include "tls.hpp"
#include "connection.hpp"
#include "url.hpp"
#include "util.hpp"

uint32_t resolve_host(const std::string& host) {
    struct addrinfo hints, *info;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family   = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    
    int error = getaddrinfo(host.c_str(), NULL, &hints, &info);
    if(error != 0) {
        std::stringstream ss;
        ss << "getaddrinfo failed: " << gai_strerror(error);
        throw ss.str();
    }
    
    uint32_t s_addr = ((struct sockaddr_in*)info->ai_addr)->sin_addr.s_addr;
    freeaddrinfo(info);
    
    return s_addr;
}

int connect_to_server(const std::string& host, int port) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(struct sockaddr_in));
    server_addr.sin_addr.s_addr = resolve_host(host);
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if(server_fd < 0) {
        std::stringstream ss;
        ss << "socket failed: " << strerror(errno);
        throw ss.str();
    }
    
    if(connect(server_fd, (struct sockaddr*)&server_addr, sizeof(struct sockaddr_in)) != 0) {
        std::stringstream ss;
        ss << "connect failed: " << strerror(errno);
        throw ss.str();
    }
    
    return server_fd;
}

void tls_relay(Connection& src, Connection& dst) {
    TLSRecord(src).write(dst);
}

inline bool is_proxy_header(const HTTPHeader& header) {
    return str_eq_case_ins(header.first.substr(0, 5), "Proxy");
}

void unproxify(HTTP1Request& request) {
    std::stringstream ss;
    URL url = URL(request.url);
    ss << url.pathAndQuery();
    request.url = ss.str();
    request.headers.remove_if(is_proxy_header);
}

void http_relay(Connection& client, Connection& server, HTTP1Request* request) {
    std::stringstream ss;
    if(request == nullptr) {
        HTTP1Request request = HTTP1Request(client);
        unproxify(request);
        ss << std::endl << "************************************************ HTTP Request start" << std::endl << std::endl;
        ss << request;
        ss << std::endl << "************************************************ HTTP Request end" << std::endl << std::endl;
        request.write(server);
    } else {
        unproxify(*request);
        ss << std::endl << "************************************************ HTTP Request start" << std::endl << std::endl;
        ss << *request;
        ss << std::endl << "************************************************ HTTP Request end" << std::endl << std::endl;
        request->write(server);
    }
    HTTP1Response response = HTTP1Response(server);
    ss << std::endl << "************************************************ HTTP Response start" << std::endl << std::endl;
    ss << response;
    ss << std::endl << "************************************************ HTTP Response end" << std::endl << std::endl;
    response.write(client);
    std::cerr << ss.str();
}

HTTP1Response create_error_response(std::string error) {
    std::stringstream ss;
    ss << "<html><body><p>" << error << "</p></body></html>";
    std::string body_str = ss.str();
    HTTPBody body = HTTPBody(body_str.begin(), body_str.end());
    return HTTP1Response(
        "HTTP/1.1",
        "400",
        "Bad Request",
        {
            {"Content-Type", "text/html"},
            {"Content-Length", std::to_string(body.size())},
            {"Proxy-Connection", "close"}
        },
        body
    );
}

void start_relay(int client_fd) {
    Connection client;
    HTTP1Request request;
    URL url;
    Connection server;
    try {
        client = Connection(client_fd);
        request = HTTP1Request(client);
        url = URL(request.url);
        std::string host = url.host;
        int port = url.port.empty() ? 80 : std::stoi(url.port);
        int server_fd = connect_to_server(host, port);
        server = Connection(server_fd);
    } catch(std::string& error) {
        std::cerr << "ERROR " << error << std::endl;
        create_error_response(error).write(client);
        goto SHUTDOWN;
    }
    std::cerr << "OPEN  " << url << std::endl;
    if(request.method.compare("CONNECT") == 0) {
        HTTP1Response("HTTP/1.1", "200", "Connection established", {}, {}).write(client);
        std::thread client_to_server([&url, &client, &server]() {
            try {
                while (true) {
                    tls_relay(client, server);
                }
            } catch(std::string& error) {
                std::cerr << "CLOSE " << url << " " << error << std::endl;
            }
        });
        std::thread server_to_client([&server, &client]() {
            try {
                while (true) {
                    tls_relay(server, client);
                }
            } catch(std::string& error) {}
        });
        client_to_server.join();
        server_to_client.join();
    } else {
        bool is_first = true;
        try {
            while (true) {
                if(is_first) {
                    http_relay(client, server, &request);
                    is_first = false;
                } else {
                    http_relay(client, server, nullptr);
                }
            }
        } catch(std::string& error) {
            std::cerr << "CLOSE " << url << " " << error << std::endl;
        }
    }
SHUTDOWN:
    client.shutdown();
    server.shutdown();
}

void ignore_sigpipe() {
    struct sigaction action;
    action.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &action, NULL);
}

void ignore_sigchld() {
    struct sigaction action;
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART | SA_NOCLDWAIT;
    sigaction(SIGCHLD, &action, NULL);
}

void start_server(int port) {
    ignore_sigpipe();
    ignore_sigchld();
    
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if(server_fd < 0) {
        std::stringstream ss;
        ss << "socket failed: " << strerror(errno);
        throw ss.str();
    }
    
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(struct sockaddr_in));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = INADDR_ANY;
    
    if(bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) != 0) {
        std::stringstream ss;
        ss << "bind failed: " << strerror(errno);
        throw ss.str();
    }
    
    if(listen(server_fd, 5) != 0) {
        std::stringstream ss;
        ss << "listen failed: " << strerror(errno);
        throw ss.str();
    }

    std::cout << "proxy server started at " << port << std::endl << std::endl;
    
    while (true) {
        struct sockaddr_in client_addr;
        memset(&client_addr, 0, sizeof(struct sockaddr_in));
        socklen_t client_addr_len = sizeof(struct sockaddr_in);
        int client_fd = accept(server_fd, (struct sockaddr*)&client_addr, &client_addr_len);
        if(client_fd < 0) {
            std::cerr << "ERROR accept failed: " << strerror(errno) << std::endl;
            continue;
        }
        int pid = fork();
        if(pid < 0) continue;
        if(pid == 0) {
            try {
                start_relay(client_fd);
            } catch(std::string& error) {
                std::cerr << "ERROR " << error << std::endl;
            }
            exit(0);
        };
    }
}

int main(int argc, const char * argv[]) {
    try {
        int port_number;
        switch(argc) {
            case 1:
                port_number = 8080;
                break;
            case 2:
                port_number = atoi(argv[1]);
                if(port_number == 0) {
                    throw std::string("An invalid port number. Please pass an integer for the port number.");
                }
                break;
            default:
                throw std::string("Too many arguments. This program accepts only 1 argument. That is the port number which this server listens.");
        }
        start_server(port_number);
    } catch(std::string& error) {
        std::cerr << "FATAL " << error << std::endl;
        return 1;
    }
    
    return 0;
}
