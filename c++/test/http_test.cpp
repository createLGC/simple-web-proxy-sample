#include "../http.hpp"
#include "FILEObj.hpp"

#include <cstdio>
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>

static void test_CONNECT() {
    const char* request_data =
        "CONNECT www.google.com:443 HTTP/1.1\r\n"
        "Proxy-Connection: keep-alive\r\n"
        "\r\n";

    auto f = FILEObj(request_data, strlen(request_data));

    auto conn = Connection(f.descriptor());
    
    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"proxy-connection", "keep-alive"}};
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

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"accept-encoding", "gzip, deflate, br"}};
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

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"content-length", std::to_string(body.size())}};
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

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"transfer-encoding", "chunked"}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_no_method() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "/path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"content-length", std::to_string(body.size())}};
    assert(request.method == "/path1/path2?a=b&c=d" && request.url == "HTTP/1.1" && request.version.empty() && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_no_url() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"content-length", std::to_string(body.size())}};
    assert(request.method == "POST" && request.url == "HTTP/1.1" && request.version.empty() && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_no_version() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"content-length", std::to_string(body.size())}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version.empty() && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_no_value_header() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection\r\n"
        "Content-Type: \r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", ""}, {"content-type", ""}, {"content-length", std::to_string(body.size())}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_no_empty_line() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::string& error) {
        assert(error == "connection closed");
    } catch(...) {
        assert(false);
    }
}

static void test_invalid_content_length() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: aiueo\r\n"
        "\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::invalid_argument&) {
    } catch(...) {
        assert(false);
    }
}

static void test_too_long_content_length() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Content-Length: 100\r\n\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::string& error) {
        assert(error == "connection closed");
    } catch(...) {
        assert(false);
    }
}

static void test_not_chunked_transfer_encoding() {
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
        "Transfer-Encoding: deflate, gzip\r\n"
        "\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::string& error) {
        constexpr char error_text[] = "not implemented transfer-encoding";
        assert(std::equal(error_text, error_text + sizeof(error_text) - 1, std::begin(error)));
    } catch(...) {
        assert(false);
    }
}

static void test_invalid_chunk_size_transfer_encoding() {
    std::string body =
        "ggfdg\r\n"
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

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::invalid_argument&) {
    } catch(...) {
        assert(false);
    }
}

static void test_too_long_chunk_size_transfer_encoding() {
    std::string body =
        "ff\r\n"
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

    try {
        auto request = HTTP1Request(conn);
        assert(false);
    } catch(const std::string& error) {
        assert(error == "connection closed");
    } catch(...) {
        assert(false);
    }
}

static void test_too_short_chunk_size_transfer_encoding() {
    std::string body =
        "1\r\n"
        "{ \"a\": \"b\", \"\r\n"
        "2\r\n"
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

    /*
    行ごとに読んでいるのでchunk sizeが短くてもうまくいってしまう
    */
    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"transfer-encoding", "chunked"}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_transfer_encoding_with_trailers() {
    std::string body =
        "d\r\n"
        "{ \"a\": \"b\", \"\r\n"
        "f\r\n"
        "c\": [\"d\", \"e\"]}\r\n"
        "0\r\n"
        "Expires: Wed, 21 Oct 2015 07:28:00 GMT\r\n"
        "\r\n";
    std::string request_data = 
        "POST /path1/path2?a=b&c=d HTTP/1.1\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Transfer-Encoding: chunked\r\n"
        "Trailer: Expires\r\n"
        "\r\n" + body;

    auto f = FILEObj(request_data.data(), request_data.size());

    auto conn = Connection(f.descriptor());

    auto request = HTTP1Request(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"transfer-encoding", "chunked"}, {"trailer", "Expires"}};
    assert(request.method == "POST" && request.url == "/path1/path2?a=b&c=d" && request.version == "HTTP/1.1" && request.headers == headers && request.body == HTTPBody(body.begin(), body.end()));
}

static void test_unproxify() {
    auto request = HTTP1Request("GET", "http://www.example.com/path1/path2/?a=b&c=d", "HTTP/1.1", {{"connection", "keep-alive"}, {"proxy-connection", "keep-alive"}}, {});

    request.unproxify();

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}};
    assert(request.method == "GET" && request.url == "/path1/path2?a=b&c=d" && request.headers == headers && request.body.empty());
}

static void test_200_OK() {
    std::string body = "{ \"a\": \"b\", \"c\": [\"d\", \"e\"]}";
    std::string response_data = 
        "HTTP/1.1 200 OK\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Set-Cookie: p=gjtu495ri3o4r8394rgwje03453; Domain=www.google.com; Secure; HttpOnly\r\n"
        "Set-Cookie: q=frnjwf4384tuerngdveo4t39ngri394; Domain=www.google.com; Secure; HttpOnly\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;

    auto f = FILEObj(response_data.data(), response_data.size());

    auto conn = Connection(f.descriptor());

    auto response = HTTP1Response(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"set-cookie", "p=gjtu495ri3o4r8394rgwje03453; Domain=www.google.com; Secure; HttpOnly"}, {"set-cookie", "q=frnjwf4384tuerngdveo4t39ngri394; Domain=www.google.com; Secure; HttpOnly"}, {"content-length", std::to_string(body.size())}};
    assert(response.version == "HTTP/1.1" && response.status_code == "200" && response.status_text == "OK" && response.headers == headers && response.body == HTTPBody(body.begin(), body.end()));
}

static void test_204_No_Content() {
    std::string response_data = 
        "HTTP/1.1 204 No Content\r\n"
        "\r\n";

    auto f = FILEObj(response_data.data(), response_data.size());

    auto conn = Connection(f.descriptor());

    auto response = HTTP1Response(conn);

    assert(response.version == "HTTP/1.1" && response.status_code == "204" && response.status_text == "No Content" && response.headers.empty() && response.body.empty());
}

static void test_200_OK_transfer_encoding() {
    std::string body =
        "d\r\n"
        "{ \"a\": \"b\", \"\r\n"
        "f\r\n"
        "c\": [\"d\", \"e\"]}\r\n"
        "0\r\n"
        "\r\n";
    std::string response_data = 
        "HTTP/1.1 200 OK\r\n"
        "Connection: keep-alive\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Set-Cookie: p=gjtu495ri3o4r8394rgwje03453; Domain=www.google.com; Secure; HttpOnly\r\n"
        "Set-Cookie: q=frnjwf4384tuerngdveo4t39ngri394; Domain=www.google.com; Secure; HttpOnly\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n" + body;

    auto f = FILEObj(response_data.data(), response_data.size());

    auto conn = Connection(f.descriptor());

    auto response = HTTP1Response(conn);

    auto headers = std::multimap<std::string, std::string>{{"connection", "keep-alive"}, {"content-type", "application/json; charset=utf-8"}, {"set-cookie", "p=gjtu495ri3o4r8394rgwje03453; Domain=www.google.com; Secure; HttpOnly"}, {"set-cookie", "q=frnjwf4384tuerngdveo4t39ngri394; Domain=www.google.com; Secure; HttpOnly"}, {"transfer-encoding", "chunked"}};
    assert(response.version == "HTTP/1.1" && response.status_code == "200" && response.status_text == "OK" && response.headers == headers && response.body == HTTPBody(body.begin(), body.end()));
}

void test_http() {
    std::cerr << "****** start test_http ******" << std::endl;
    test_CONNECT();
    test_GET();
    test_POST();
    test_POST_transfer_encoding();
    test_no_method();
    test_no_url();
    test_no_version();
    test_no_value_header();
    test_no_empty_line();
    test_invalid_content_length();
    test_too_long_content_length();
    test_not_chunked_transfer_encoding();
    test_invalid_chunk_size_transfer_encoding();
    test_too_long_chunk_size_transfer_encoding();
    test_too_short_chunk_size_transfer_encoding();
    test_transfer_encoding_with_trailers();
    test_unproxify();
    test_200_OK();
    test_204_No_Content();
    test_200_OK_transfer_encoding();
    std::cerr << "****** end test_http ******" << std::endl;
}