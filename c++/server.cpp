#include <cassert>
#include <iostream>
#include <thread>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <unistd.h>

#include "tls.hpp"
#include "url.hpp"
#include "util.hpp"
#include "server.hpp"

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
    TLSRecord(src) >> dst;
}

void http_relay(Connection& client, Connection& server, HTTP1Request* request) {
    std::stringstream ss;
    if(request == nullptr) {
        HTTP1Request request = HTTP1Request(client);
        request.unproxify();
        ss << std::endl << "************************************************ HTTP Request start" << std::endl << std::endl;
        ss << request;
        ss << std::endl << "************************************************ HTTP Request end" << std::endl << std::endl;
        request >> server;
    } else {
        request->unproxify();
        ss << std::endl << "************************************************ HTTP Request start" << std::endl << std::endl;
        ss << *request;
        ss << std::endl << "************************************************ HTTP Request end" << std::endl << std::endl;
        *request >> server;
    }
    HTTP1Response response = HTTP1Response(server);
    ss << std::endl << "************************************************ HTTP Response start" << std::endl << std::endl;
    ss << response;
    ss << std::endl << "************************************************ HTTP Response end" << std::endl << std::endl;
    response >> client;
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
    std::unique_ptr<Connection> client_p, server_p;
    HTTP1Request request;
    URL url;
    try {
        client_p = std::make_unique<Connection>(client_fd);
        request = HTTP1Request(*client_p);
        url = URL(request.url);
        std::string host = url.host;
        int port = url.port.empty() ? 80 : std::stoi(url.port);
        int server_fd = connect_to_server(host, port);
        server_p = std::make_unique<Connection>(server_fd);
    } catch(std::string& error) {
        std::cerr << "ERROR " << error << std::endl;
        if(client_p) {
            create_error_response(error) >> *client_p;
            client_p->shutdown();
        }
        assert(server_p == nullptr);
        throw error;
    }
    Connection client = *client_p, server = *server_p;
    std::cerr << "OPEN  " << url << std::endl;
    if(request.method == "CONNECT") {
        HTTP1Response("HTTP/1.1", "200", "Connection established", {}, {}) >> client;
        std::thread client_to_server([&url, &client, &server]() {
            try {
                while (true) {
                    tls_relay(client, server);
                }
            } catch(std::string& error) {
                std::cerr << "CLOSE " << url << " " << error << std::endl;
            } catch(...) {
                std::cerr << "CLOSE " << url << std::endl;
            }
        });
        std::thread server_to_client([&server, &client]() {
            try {
                while (true) {
                    tls_relay(server, client);
                }
            } catch(...) {}
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
        } catch(...) {
            std::cerr << "CLOSE " << url << std::endl;
        }
    }
    client.shutdown();
    server.shutdown();
}

static void ignore_sigpipe() {
    struct sigaction action;
    action.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &action, NULL);
}

static void ignore_sigchld() {
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

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    
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
        else if(pid == 0) {
            try {
                start_relay(client_fd);
            } catch(std::string& error) {
                std::cerr << "ERROR " << error << std::endl;
            } catch(...) {
                std::cerr << "ERROR" << std::endl;
            }
            exit(0);
        }
    }
}