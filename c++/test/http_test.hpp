#include "../http.hpp"

#include <stdio.h>
#include <unistd.h>
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>

class FILEObj {
private:
    FILE* fp;

    FILEObj(FILE* fp): fp(fp) {}

public:
    FILEObj(const void* ptr, size_t size): FILEObj(tmpfile()) {
        fwrite(ptr, 1, size, fp);
        fflush(fp);
        fseek(fp, 0, SEEK_SET);
    }

    ~FILEObj() { close(); }

    FILEObj(const FILEObj&) = delete;
    FILEObj& operator=(const FILEObj&) = delete;

    FILEObj(FILEObj&&) = default;
    FILEObj& operator=(FILEObj&&) = default;

    int descriptor() const { return fileno(fp); }

    bool eof() const { return feof(fp) != 0; }

    void close() { fclose(fp); }
};

static void test_CONNECT() {
    const char* request_data =
        "CONNECT www.google.com:443 HTTP/1.1\r\n"
        "Proxy-Connection: keep-alive\r\n"
        "\r\n";

    auto f = FILEObj(request_data, strlen(request_data));

    auto conn = Connection(f.descriptor());
    
    auto request = HTTP1Request(conn);

    auto headers = std::vector<HTTPHeader>{{"Proxy-Connection", "keep-alive"}};
    assert(request.method == "CONNECT" && request.url == "www.google.com:443" && request.version == "HTTP/1.1" && request.headers == headers && request.body.empty());
}

static void test_GET() {
    const char* request_data =
        "GET /path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Accept-Encoding: gzip, deflate, br\r\n"
        "\r\n";

    auto f = FILEObj(request_data, strlen(request_data));

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::vector<HTTPHeader>{{"Connection", "keep-alive"}, {"Accept-Encoding", "gzip, deflate, br"}};
    assert(request.method == "GET" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body.empty());
}

static void test_POST() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::vector<HTTPHeader>{{"Connection", "keep-alive"}, {"Content-Type", "application/json; charset=utf-8"}, {"Content-Length", std::to_string(body.size())}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_POST_transfer_encoding() {
    std::string body =
        "d\r\n"
        "{ \"a\": \"b\", \"\r\n"
        "f\r\n"
        "c\": [\"d\", \"e\"]}\r\n"
        "0\r\n"
        "\r\n";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::vector<HTTPHeader>{{"Connection", "keep-alive"}, {"Content-Type", "application/json; charset=utf-8"}, {"Transfer-Encoding", "chunked"}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

void test_HTTP1Request() {
    test_CONNECT();
    test_GET();
    test_POST();
    test_POST_transfer_encoding();
}