#ifndef url_hpp
#define url_hpp

#include <stdio.h>
#include <iostream>
#include <string>

class URL {
public:
    std::string scheme;
    std::string user;
    std::string password;
    std::string host;
    std::string port;
    std::vector<std::string> path;
    std::vector<std::pair<std::string, std::string>> query;
    std::string anchor;
    
    URL() {}
    URL(std::string urlString);
    URL(std::string scheme, std::string user, std::string password, std::string host, std::string port, std::vector<std::string> path, std::vector<std::pair<std::string, std::string>> query, std::string anchor);
    URL pathAndQuery() const;
};

std::ostream& operator<<(std::ostream& out, const URL& url);

#endif
