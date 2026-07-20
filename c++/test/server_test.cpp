#include "../server.hpp"
#include "FILEObj.hpp"
#include "../tls.hpp"

#include <cassert>
#include <iostream>
#include <thread>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <unistd.h>

static void create_test_server(int port, std::function<bool(int)> callback) {
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

    std::cout << "test server started at " << port << std::endl << std::endl;

    int pid = fork();
    if(pid == 0) {
        bool ended = false;
        while(!ended) {
            struct sockaddr_in client_addr;
            memset(&client_addr, 0, sizeof(struct sockaddr_in));
            socklen_t client_addr_len = sizeof(struct sockaddr_in);
            int client_fd = accept(server_fd, (struct sockaddr*)&client_addr, &client_addr_len);
            if(client_fd < 0) {
                std::cerr << "ERROR accept failed: " << strerror(errno) << std::endl;
                continue;
            }
            std::thread th([&ended, &callback, client_fd]{
                ended = callback(client_fd);
                close(client_fd);
            });
            th.join();
        }
        close(server_fd);
    }
}

static int create_test_client(int port) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(struct sockaddr_in));
    server_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
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

static void test_resolve_host() {
    uint32_t _addr = resolve_host("www.google.com"); //142.251.150~157.119
    uint8_t* addr = reinterpret_cast<uint8_t*>(&_addr);
    assert(addr[0] == 142 && addr[1] == 251 && addr[3] == 119);
    for(int i = 0; i < 8; i++) {
        if(addr[2] == 150 + i) return;
    }
    assert(false);
}

static void test_connect_to_server() {
    int server_fd;
    try {
        server_fd = connect_to_server("www.google.com", 443);
    } catch(const std::string& error) {
        std::cerr << error << std::endl;
        assert(false);
    }
    close(server_fd);
}

static void test_tls_relay() {
    const std::vector<char> record_data{ 21, 0x03, 0x03, 0, 2, 0, 0, 22, 0x03, 0x02, 0, 3, 1, 2, 3 };

    auto src = FILEObj(record_data.data(), record_data.size());
    auto src_conn = Connection(src.descriptor());

    auto dst = FILEObj();
    auto dst_conn = Connection(dst.descriptor());

    tls_relay(src_conn, dst_conn);
    tls_relay(src_conn, dst_conn);
    dst.seekToHead();

    std::vector<char> buf(record_data.size());
    dst_conn.read(buf.data(), 1, record_data.size());

    assert(record_data == buf);
}

static void test_http_relay() {
    constexpr int port = 8081;
    auto request = HTTP1Request("POST", "http://www.example.com/path1/path2?a=b&c=d", "HTTP/1.1", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    auto response = HTTP1Response("HTTP/1.1", "200", "OK", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    FILE* result_fp = tmpfile();
    create_test_server(port, [&request, &response, result_fp](int client_fd){
        auto client = Connection(client_fd);

        auto received_request = HTTP1Request(client);
        request.unproxify();

        std::cerr << request << std::endl;

        bool result = received_request.method == request.method && received_request.url == request.url && received_request.version == request.version && received_request.headers == request.headers && received_request.body == request.body;
        fwrite(&result, 1, 1, result_fp);
        fflush(result_fp);
        fseek(result_fp, 0, SEEK_SET);

        response >> client;
        client.flush();

        return true;
    });

    auto client = FILEObj();
    auto client_conn = Connection(client.descriptor());
    client_conn << request;
    client.seekToHead();

    int server_fd = create_test_client(port);
    auto server_conn = Connection(server_fd);

    http_relay(client_conn, server_conn, nullptr);
    bool result = false;
    fread(&result, 1, 1, result_fp);
    assert(result);
    fclose(result_fp);
    client.seekToHead();
    close(server_fd);

    auto _ = HTTP1Request(client_conn);
    auto response2 = HTTP1Response(client_conn);
    assert(response.version == response2.version && response.status_code == response2.status_code && response.status_text == response2.status_text && response.headers == response2.headers && response.body == response2.body);
}

static void test_http_relay_with_request() {
    constexpr int port = 8082;
    auto request = HTTP1Request("POST", "http://www.example.com/path1/path2?a=b&c=d", "HTTP/1.1", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    auto response = HTTP1Response("HTTP/1.1", "200", "OK", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    FILE* result_fp = tmpfile();
    create_test_server(port, [&request, &response, result_fp](int client_fd){
        auto client = Connection(client_fd);

        auto received_request = HTTP1Request(client);
        request.unproxify();

        bool result = received_request.method == request.method && received_request.url == request.url && received_request.version == request.version && received_request.headers == request.headers && received_request.body == request.body;
        fwrite(&result, 1, 1, result_fp);
        fflush(result_fp);
        fseek(result_fp, 0, SEEK_SET);

        response >> client;
        client.flush();

        return true;
    });

    auto client = FILEObj();
    auto client_conn = Connection(client.descriptor());

    int server_fd = create_test_client(port);
    auto server_conn = Connection(server_fd);

    http_relay(client_conn, server_conn, &request);
    bool result = false;
    fread(&result, 1, 1, result_fp);
    assert(result);
    fclose(result_fp);
    client.seekToHead();
    close(server_fd);

    auto response2 = HTTP1Response(client_conn);
    assert(response.version == response2.version && response.status_code == response2.status_code && response.status_text == response2.status_text && response.headers == response2.headers && response.body == response2.body);
}

static void test_start_server_http() {
    constexpr int proxy_port = 8080;
    constexpr int server_port = 8081;

    std::stringstream url;
    url << "http://localhost:" << server_port << "/path1/path2?a=b&c=d";
    
    auto request = HTTP1Request("POST", url.str(), "HTTP/1.1", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    auto response = HTTP1Response("HTTP/1.1", "200", "OK", {{"connection", "keep-alive"}, {"content-length", "5"}}, {'a', 'i', 'u', 'e', 'o'});
    FILE* result_fp = tmpfile();
    create_test_server(server_port, [&request, &response, result_fp](int client_fd){
        auto client = Connection(client_fd);

        auto received_request = HTTP1Request(client);
        auto _request = request;
        _request.unproxify();

        bool result = received_request.method == _request.method && received_request.url == _request.url && received_request.version == _request.version && received_request.headers == _request.headers && received_request.body == _request.body;
        fwrite(&result, 1, 1, result_fp);
        fflush(result_fp);
        fseek(result_fp, 0, SEEK_SET);

        response >> client;
        client.flush();

        return true;
    });
    int server_fd = create_test_client(server_port);
    auto server_conn = Connection(server_fd);

    int pid = fork();
    if(pid == 0) {
        start_server(proxy_port);
    } else if(pid > 0) {
        request >> server_conn;
        auto response2 = HTTP1Response(server_conn);
        bool result = false;
        fread(&result, 1, 1, result_fp);
        fclose(result_fp);
        assert(result);
        assert(response.version == response2.version && response.status_code == response2.status_code && response.status_text == response2.status_text && response.headers == response2.headers && response.body == response2.body);
        std::stringstream ss;
        ss << "lsof -t -i:" << proxy_port << " | xargs kill -9";
        system(ss.str().c_str());
    }
}

static void test_start_server_tls() {
    constexpr int proxy_port = 8080;
    constexpr int server_port = 8081;

    const char record_data[] = { 21, 0x03, 0x03, 0, 2, 0, 0 };

    FILE* result_fp = tmpfile();
    create_test_server(server_port, [&request, &response, result_fp](int client_fd){
        auto client = Connection(client_fd);

        auto received_request = HTTP1Request(client);
        auto _request = request;
        _request.unproxify();

        bool result = received_request.method == _request.method && received_request.url == _request.url && received_request.version == _request.version && received_request.headers == _request.headers && received_request.body == _request.body;
        fwrite(&result, 1, 1, result_fp);
        fflush(result_fp);
        fseek(result_fp, 0, SEEK_SET);

        response >> client;
        client.flush();

        return true;
    });
    int server_fd = create_test_client(server_port);
    auto server_conn = Connection(server_fd);

    int pid = fork();
    if(pid == 0) {
        start_server(proxy_port);
    } else if(pid > 0) {
        request >> server_conn;
        auto response2 = HTTP1Response(server_conn);
        bool result = false;
        fread(&result, 1, 1, result_fp);
        fclose(result_fp);
        assert(result);
        assert(response.version == response2.version && response.status_code == response2.status_code && response.status_text == response2.status_text && response.headers == response2.headers && response.body == response2.body);
        std::stringstream ss;
        ss << "lsof -t -i:" << proxy_port << " | xargs kill -9";
        system(ss.str().c_str());
    }
}

void test_server() {
    std::cerr << "****** start test_server ******" << std::endl;
    test_resolve_host();
    test_connect_to_server();
    test_tls_relay();
    test_http_relay();
    test_http_relay_with_request();
    std::cerr << "****** end test_server ******" << std::endl;
}