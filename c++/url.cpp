#include <sstream>
#include "url.hpp"

static std::string cutScheme(std::string& urlString) {
    size_t schemeSeparatorIndex = urlString.find("//");
    if(schemeSeparatorIndex == std::string::npos) { //ない場合
        return "";
    }
    
    if(schemeSeparatorIndex == 0) { //"//"の場合
        urlString.erase(0, 2);
        return "";
    }
    
    if(urlString.at(schemeSeparatorIndex - 1) != ':' || schemeSeparatorIndex == 1) {
        throw std::string("invalid scheme");
    }
    
    std::string scheme = urlString.substr(0, schemeSeparatorIndex - 1);
    urlString.erase(0, schemeSeparatorIndex + 2);
    return scheme;
}

static std::pair<std::string, std::string> cutUserAndPassword(std::string& urlString) {
    size_t atmarkIndex = urlString.find("@");
    if(atmarkIndex == std::string::npos) {
        return {"", ""};
    }
    
    std::string userAndPassword = urlString.substr(0, atmarkIndex);
    urlString.erase(0, atmarkIndex + 1);
    
    size_t colonIndex = userAndPassword.find(":");
    if(colonIndex == std::string::npos) {
        throw std::string("no colon");
    }
    
    std::string user = userAndPassword.substr(0, colonIndex);
    std::string password;
    if(colonIndex + 1 < userAndPassword.size()) {
        password = userAndPassword.substr(colonIndex + 1);
    }
    
    return {user, password};
}

static std::pair<std::string, std::string> cutHostAndPort(std::string& urlString) {
    std::string hostAndPort;
    size_t slashIndex = urlString.find("/");
    if(slashIndex == std::string::npos) {
        hostAndPort = urlString;
        urlString.erase();
    } else {
        hostAndPort = urlString.substr(0, slashIndex);
        urlString.erase(0, slashIndex + 1); //パスの先頭の/も消してる
    }
    
    std::string host;
    std::string port;
    size_t colonIndex = hostAndPort.find(":");
    if(colonIndex == std::string::npos) {
        host = hostAndPort;
    } else {
        host = hostAndPort.substr(0, colonIndex);
        if(colonIndex + 1 < hostAndPort.size()) {
            port = hostAndPort.substr(colonIndex + 1);
        }
    }
    
    return {host, port};
}

static std::list<std::string> cutPath(std::string& urlString) {
    std::string pathString;
    size_t questionIndex = urlString.find("?");
    if(questionIndex == std::string::npos) {
        pathString = urlString;
        urlString.erase();
    } else {
        pathString = urlString.substr(0, questionIndex);
        urlString.erase(0, questionIndex + 1); //クエリの先頭の?も消してる
    }
    
    std::list<std::string> path;
    std::stringstream ss{pathString};
    while (true) {
        std::string _path;
        std::getline(ss, _path, '/');
        if(_path.empty()) break;
        path.push_back(_path);
    }
    
    return path;
}

static std::list<std::pair<std::string, std::string>> cutQuery(std::string& urlString) {
    std::string queryString;
    size_t hashIndex = urlString.find("#");
    if(hashIndex == std::string::npos) {
        queryString = urlString;
        urlString.erase();
    } else {
        queryString = urlString.substr(0, hashIndex);
        urlString.erase(0, hashIndex + 1); //#も消してる
    }
    
    std::list<std::pair<std::string, std::string>> query;
    std::stringstream ss{queryString};
    while (true) {
        std::string _query;
        std::getline(ss, _query, '&');
        if(_query.empty()) break;
        std::string key;
        std::string value;
        size_t equalIndex = _query.find("=");
        if(equalIndex == std::string::npos) {
            key = _query;
        } else {
            key = _query.substr(0, equalIndex);
            if(equalIndex + 1 < _query.size()) {
                value = _query.substr(equalIndex + 1);
            }
        }
        query.push_back({key, value});
    }

    return query;
}

URL::URL(std::string urlString) {
    std::string scheme = cutScheme(urlString);
    auto [user, password] = cutUserAndPassword(urlString);
    auto [host, port] = cutHostAndPort(urlString);
    auto path = cutPath(urlString);
    auto query = cutQuery(urlString);
    std::string anchor = urlString;
    
    this->scheme = scheme;
    this->user = user;
    this->password = password;
    this->host = host;
    this->port = port;
    this->path = path;
    this->query = query;
    this->anchor = anchor;
}

URL::URL(std::string scheme, std::string user, std::string password, std::string host, std::string port, std::list<std::string> path, std::list<std::pair<std::string, std::string>> query, std::string anchor): scheme(scheme), user(user), password(password), host(host), port(port), path(path), query(query), anchor(anchor) {}

URL URL::pathAndQuery() {
    return URL("", "", "", "", "", path, query, "");
}

std::ostream& operator<<(std::ostream& out, const URL& url) {
    if(!url.scheme.empty()) {
        out << url.scheme << "://";
    }
    if(!url.user.empty()) {
        out << url.user;
        if(!url.password.empty()) {
            out << ":";
            out << url.password;
        }
        out << "@";
    }
    if(!url.host.empty()) {
        out << url.host;
        if(!url.port.empty()) out << ":" << url.port;
    }
    if(url.path.empty()) {
        if(url.query.empty() && url.scheme.empty() && !url.host.empty()) {
            // CONNECTの時。"/"は出力しない
        } else {
            out << "/";
        }
    } else {
        for(auto _path: url.path) {
            out << "/" << _path;
        }
    }
    if(!url.query.empty()) {
        auto itr = url.query.begin();
        out << "?" << itr->first << "=" << itr->second;
        itr++;
        for(; itr != url.query.end(); itr++) {
            out << "&" << itr->first << "=" << itr->second;
        }
    }
    if(!url.anchor.empty()) {
        out << "#" << url.anchor;
    }
    return out;
}
