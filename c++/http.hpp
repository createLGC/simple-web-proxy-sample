#ifndef http_hpp
#define http_hpp

#include "connection.hpp"

#include <string>
#include <map>
#include <vector>

using HTTPBody = std::vector<uint8_t>;

class HTTP1Request {
public:
    std::string method;
    std::string url;
    std::string version;
    std::multimap<std::string, std::string> headers;
    HTTPBody body;
    
    HTTP1Request() {}
    HTTP1Request(std::string method, std::string url, std::string version, std::multimap<std::string, std::string> headers, HTTPBody body);
    HTTP1Request(Connection& conn);
    void unproxify();
    void operator>>(Connection& conn) const;
};

void operator<<(Connection& conn, const HTTP1Request& request);
std::ostream& operator<<(std::ostream& out, const HTTP1Request& request);

class HTTP1Response {
public:
    std::string version;
    std::string status_code;
    std::string status_text;
    std::multimap<std::string, std::string> headers;
    HTTPBody body;
    
    HTTP1Response(std::string version, std::string status_code, std::string status_text, std::multimap<std::string, std::string> headers, HTTPBody body);
    HTTP1Response(Connection& conn);
    void operator>>(Connection& conn) const;
};

void operator<<(Connection& conn, const HTTP1Response& request);
std::ostream& operator<<(std::ostream& out, const HTTP1Response& response);

#endif