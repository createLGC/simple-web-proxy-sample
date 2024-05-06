#ifndef http_hpp
#define http_hpp

#include <stdio.h>
#include <string>
#include <list>
#include <vector>
#include "connection.hpp"

typedef std::pair<std::string, std::string> HTTPHeader;
typedef std::vector<uint8_t> HTTPBody;

class HTTP1Request {
public:
    std::string method;
    std::string url;
    std::string version;
    std::list<HTTPHeader> headers;
    HTTPBody body;
    
    HTTP1Request() {}
    HTTP1Request(Connection& conn);
    void write(Connection& conn);
};

std::ostream& operator<<(std::ostream& out, const HTTP1Request& request);

class HTTP1Response {
public:
    std::string version;
    std::string status_code;
    std::string status_text;
    std::list<HTTPHeader> headers;
    HTTPBody body;
    
    HTTP1Response(std::string version, std::string status_code, std::string status_text, std::list<HTTPHeader> headers, HTTPBody body);
    HTTP1Response(Connection& conn);
    void write(Connection& conn);
};

std::ostream& operator<<(std::ostream& out, const HTTP1Response& response);

#endif