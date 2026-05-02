#ifndef url_hpp
#define url_hpp

#include <iostream>
#include <string>
#include <vector>
#include <map>

class URL {
public:
    std::string scheme;
    std::string user;
    std::string password;
    std::string host;
    std::string port;
    std::vector<std::string> path;
    std::multimap<std::string, std::string> query;
    std::string anchor;
    
    URL() {}
    URL(std::string urlString);
    URL(std::string scheme, std::string user, std::string password, std::string host, std::string port, std::vector<std::string> path, std::multimap<std::string, std::string> query, std::string anchor);
    URL pathAndQuery() const;
};

bool operator==(const URL& l, const URL& r);
std::ostream& operator<<(std::ostream& out, const URL& url);

#endif
