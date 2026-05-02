#ifndef http_hpp
#define http_hpp

#include "connection.hpp"

#include <string>
#include <vector>

using HTTPHeader = std::pair<std::string, std::string>;
using HTTPBody = std::vector<uint8_t>;

class HTTP1Request {
public:
    std::string method;
    std::string url;
    std::string version;
    std::vector<HTTPHeader> headers;
    HTTPBody body;
    
    HTTP1Request() {}
    HTTP1Request(Connection& conn);
    void write(Connection& conn) const;
};

std::ostream& operator<<(std::ostream& out, const HTTP1Request& request);

class HTTP1Response {
public:
    std::string version;
    std::string status_code;
    std::string status_text;
    std::vector<HTTPHeader> headers;
    HTTPBody body;
    
    HTTP1Response(std::string version, std::string status_code, std::string status_text, std::vector<HTTPHeader> headers, HTTPBody body);
    HTTP1Response(Connection& conn);
    void write(Connection& conn) const;
};

std::ostream& operator<<(std::ostream& out, const HTTP1Response& response);

#endif