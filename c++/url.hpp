#ifndef url_hpp
#define url_hpp

#include <stdio.h>
#include <iostream>
#include <string>
#include <list>

class URL {
public:
    std::string scheme;
    std::string user;
    std::string password;
    std::string host;
    std::string port;
    std::list<std::string> path;
    std::list<std::pair<std::string, std::string>> query;
    std::string anchor;
    
    URL() {}
    URL(std::string urlString);
    URL(std::string scheme, std::string user, std::string password, std::string host, std::string port, std::list<std::string> path, std::list<std::pair<std::string, std::string>> query, std::string anchor);
    URL pathAndQuery();
};

std::ostream& operator<<(std::ostream& out, const URL& url);

#endif
