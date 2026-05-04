#include "http.hpp"
#include "util.hpp"
#include "url.hpp"

#include <iostream>
#include <sstream>

#define MAX_LINE_LENGTH 1048575

static std::tuple<std::string, std::string, std::string> parse_first_line(Connection& conn) {
    auto line = conn.readline();
    
    std::stringstream first_line{line};
    
    std::string first;
    std::getline(first_line, first, ' ');
    trim(first);
    
    std::string second;
    std::getline(first_line, second, ' ');
    trim(second);
    
    std::string third;
    std::getline(first_line, third);
    trim(third);

    return std::make_tuple(first, second, third);
}

static std::multimap<std::string, std::string> parse_headers(Connection& conn) {
    std::multimap<std::string, std::string> headers;

    while(true) {
        auto line = conn.readline();
        
        auto header_line = line;
        trim(header_line);
        
        if(header_line.empty()) break;
        
        std::stringstream ss{header_line};
        
        std::string key;
        std::getline(ss, key, ':');
        trim(key);
        transform(key.begin(), key.end(), key.begin(), tolower);
        
        std::string value;
        std::getline(ss, value);
        trim(value);
        
        headers.emplace(key, value);
    }

    return headers;
}

static HTTPBody parse_transfer_encoding(Connection& conn) {
    HTTPBody body;
    while (true) {
        auto line = conn.readline();
        auto chunk_size_line = line;
        rtrim(chunk_size_line);
        size_t chunk_size = std::stoul(chunk_size_line, nullptr, 16);

        std::copy(line.cbegin(), line.cend(), std::back_inserter(body));

        if(chunk_size == 0) {
            while(true) {
                auto line = conn.readline();
                std::copy(line.cbegin(), line.cend(), std::back_inserter(body));
                if(line == "\r\n") break;
            }
            break;
        } else {
            std::vector<uint8_t> chunk(chunk_size);
            conn.read(chunk.data(), 1, chunk_size);
            std::copy(chunk.cbegin(), chunk.cend(), std::back_inserter(body));

            auto line = conn.readline();
            std::copy(line.cbegin(), line.cend(), std::back_inserter(body));
        }
    }
    return body;
}

static HTTPBody parse_body(Connection& conn, std::multimap<std::string, std::string>& headers) {
    HTTPBody body;

    size_t body_size = 0;
    bool transfer_encoding = false;
    std::multimap<std::string, std::string>::iterator itr;
    if((itr = headers.find("content-length")) != headers.end()) {
        body_size = std::stol(itr->second);
    } else if((itr = headers.find("transfer-encoding")) != headers.end()){
        std::stringstream ss{itr->second};
        std::string value;
        while(std::getline(ss, value, ','));
        trim(value);
        if(value != "chunked") {
            std::stringstream ss;
            ss << "not implemented transfer-encoding: " << itr->second;
            throw ss.str();
        }
        transfer_encoding = true;
    }
    
    if(transfer_encoding) {
        body = parse_transfer_encoding(conn);
    } else if(body_size > 0) {
        body.resize(body_size);
        conn.read(body.data(), 1, body_size);
    }

    return body;
}

HTTP1Request::HTTP1Request(std::string method, std::string url, std::string version, std::multimap<std::string, std::string> headers, HTTPBody body): method(method), url(url), version(version), headers(headers), body(body) {}

HTTP1Request::HTTP1Request(Connection& conn) {
    auto [method, url, version] = parse_first_line(conn);
    this->method = method;
    this->url = url;
    this->version = version;

    headers = parse_headers(conn);
    body = parse_body(conn, headers);
}

void HTTP1Request::unproxify() {
    std::stringstream ss;
    URL url = URL(this->url);
    ss << url.pathAndQuery();
    this->url = ss.str();
    for(auto itr = headers.begin(); itr != headers.end();) {
        if(std::equal(itr->first.begin(), itr->first.begin() + 5, "proxy")) {
            itr = headers.erase(itr);
        } else {
            itr++;
        }
    }
}

void HTTP1Request::operator>>(Connection& conn) const {
    conn.write("%s %s %s\r\n", method.c_str(), url.c_str(), version.c_str());
    for(auto header: headers) {
        conn.write("%s: %s\r\n", header.first.c_str(), header.second.c_str());
    }
    conn.write("\r\n");
    if(body.size() > 0) conn.write(body.data(), 1, body.size());
    conn.flush();
}

void operator<<(Connection& conn, const HTTP1Request& request) {
    request >> conn;
}

std::ostream& operator<<(std::ostream& out, const HTTP1Request& request) {
    out << request.method << " " << request.url << " " << request.version << "\r\n";
    for(auto& header: request.headers) {
        out << header.first << ": " << header.second << "\r\n";
    }
    out << "\r\n";

    if(!request.body.empty()) {
        out << std::string(request.body.begin(), request.body.end()) << std::endl;
    }

    return out;
}

HTTP1Response::HTTP1Response(std::string version, std::string status_code, std::string status_text, std::multimap<std::string, std::string> headers, HTTPBody body): version(version), status_code(status_code), status_text(status_text), headers(headers), body(body) {}

HTTP1Response::HTTP1Response(Connection& conn) {
    auto [version, status_code, status_text] = parse_first_line(conn);
    this->version = version;
    this->status_code = status_code;
    this->status_text = status_text;
    
    headers = parse_headers(conn);
    body = parse_body(conn, headers);
}

void HTTP1Response::operator>>(Connection& conn) const {
    conn.write("%s %s %s\r\n", version.c_str(), status_code.c_str(), status_text.c_str());
    for(auto header: headers) {
        conn.write("%s: %s\r\n", header.first.c_str(), header.second.c_str());
    }
    conn.write("\r\n");
    if(body.size() > 0) conn.write(body.data(), 1, body.size());
    conn.flush();
}

void operator<<(Connection& conn, const HTTP1Response& response) {
    response >> conn;
}

std::ostream& operator<<(std::ostream& out, const HTTP1Response& response) {
    out << response.version << " " << response.status_code << " " << response.status_text << "\r\n";
    for(auto& header: response.headers) {
        out << header.first << ": " << header.second << "\r\n";
    }
    out << "\r\n";

    if(!response.body.empty()) {
        out << std::string(response.body.begin(), response.body.end()) << std::endl;
    }

    return out;
}
