#include <iostream>
#include <sstream>
#include <iomanip>
#include "http.hpp"
#include "util.hpp"

#define MAX_LINE_LENGTH 1048575

std::tuple<std::string, std::string, std::string> parse_first_line(Connection& conn) {
    char line_buf[MAX_LINE_LENGTH + 1];
    memset(line_buf, 0, MAX_LINE_LENGTH + 1);
    
    conn.readline(line_buf, MAX_LINE_LENGTH);
    if(strlen(line_buf) == MAX_LINE_LENGTH) throw std::string("too long first line");
    
    std::stringstream first_line{std::string(line_buf)};
    
    std::string first;
    getline(first_line, first, ' ');
    trim(first);
    
    std::string second;
    getline(first_line, second, ' ');
    trim(second);
    
    std::string third;
    getline(first_line, third);
    trim(third);

    return std::make_tuple(first, second, third);
}

std::list<HTTPHeader> parse_headers(Connection& conn) {
    char line_buf[MAX_LINE_LENGTH + 1];
    memset(line_buf, 0, MAX_LINE_LENGTH + 1);

    std::list<HTTPHeader> headers;

    while(true) {
        memset(line_buf, 0, MAX_LINE_LENGTH + 1);
        conn.readline(line_buf, MAX_LINE_LENGTH);
        if(strlen(line_buf) == MAX_LINE_LENGTH) throw std::string("too long header line");
        
        std::string line_buf_str = line_buf;
        trim(line_buf_str);
        
        if(line_buf_str.size() == 0) break;
        
        std::stringstream line{line_buf_str};
        
        std::string key;
        getline(line, key, ':');
        trim(key);
        
        std::string value;
        getline(line, value);
        trim(value);
        
        headers.push_back({key, value});
    }

    return headers;
}

HTTPBody parse_transfer_encoding(Connection& conn) {
    HTTPBody body;
    while (true) {
        char line_buf[MAX_LINE_LENGTH + 1];
        memset(line_buf, 0, MAX_LINE_LENGTH + 1);
        conn.readline(line_buf, MAX_LINE_LENGTH);

        size_t i = 0;
        for(; line_buf[i] != ';' && line_buf[i] != '\r' && line_buf[i] != '\0' && i <= MAX_LINE_LENGTH; i++);
        
        char chunk_size_buf[i + 1];
        memcpy(chunk_size_buf, line_buf, i);
        chunk_size_buf[i] = '\0';
        size_t chunk_size = std::stoul(chunk_size_buf, nullptr, 16);

        std::copy(line_buf, line_buf + strlen(line_buf), std::back_inserter(body));

        if(chunk_size == 0) {
            while (true) {
                char line_buf[MAX_LINE_LENGTH + 1];
                memset(line_buf, 0, MAX_LINE_LENGTH + 1);
                conn.readline(line_buf, MAX_LINE_LENGTH);
                std::copy(line_buf, line_buf + strlen(line_buf), std::back_inserter(body));
                if(line_buf[0] == '\r' && line_buf[1] == '\n' && line_buf[2] == '\0') break;
            }
            break;
        } else {
            uint8_t chunk[chunk_size];
            conn.read(chunk, 1, chunk_size);

            char line_buf2[MAX_LINE_LENGTH + 1];
            memset(line_buf2, 0, MAX_LINE_LENGTH + 1);
            conn.readline(line_buf2, MAX_LINE_LENGTH);

            std::copy(chunk, chunk + chunk_size, std::back_inserter(body));
            std::copy(line_buf2, line_buf2 + strlen(line_buf2), std::back_inserter(body));
        }
    }
    return body;
}

HTTPBody parse_body(Connection& conn, std::list<HTTPHeader>& headers) {
    HTTPBody body;

    size_t body_size = 0;
    bool transfer_encoding = false;
    for(auto& header: headers) {
        if(str_eq_case_ins(header.first, "Content-Length")) {
            body_size = stol(header.second);
            break;
        }
        if(str_eq_case_ins(header.first, "Transfer-Encoding")) {
            transfer_encoding = true;
            break;
        }
    }
    
    if(transfer_encoding) {
        body = parse_transfer_encoding(conn);
    } else if(body_size > 0) {
        char buf[body_size];
        conn.read(buf, 1, body_size);
        body = HTTPBody(buf, buf + body_size);
    }

    return body;
}

HTTP1Request::HTTP1Request(Connection& conn) {
    auto [method, url, version] = parse_first_line(conn);
    this->method = method;
    this->url = url;
    this->version = version;

    headers = parse_headers(conn);
    body = parse_body(conn, headers);
}

void HTTP1Request::write(Connection& conn) {
    conn.writeline("%s %s %s\r", method.c_str(), url.c_str(), version.c_str());
    for(auto header: headers) {
        conn.writeline("%s: %s\r", header.first.c_str(), header.second.c_str());
    }
    conn.writeline("\r");
    if(body.size() > 0) conn.write(body.data(), 1, body.size());
    conn.flush();
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

HTTP1Response::HTTP1Response(std::string version, std::string status_code, std::string status_text, std::list<HTTPHeader> headers, HTTPBody body): version(version), status_code(status_code), status_text(status_text), headers(headers), body(body) {}

HTTP1Response::HTTP1Response(Connection& conn) {
    auto [version, status_code, status_text] = parse_first_line(conn);
    this->version = version;
    this->status_code = status_code;
    this->status_text = status_text;
    
    headers = parse_headers(conn);
    body = parse_body(conn, headers);
}

void HTTP1Response::write(Connection& conn) {
    conn.writeline("%s %s %s\r", version.c_str(), status_code.c_str(), status_text.c_str());
    for(auto header: headers) {
        conn.writeline("%s: %s\r", header.first.c_str(), header.second.c_str());
    }
    conn.writeline("\r");
    if(body.size() > 0) conn.write(body.data(), 1, body.size());
    conn.flush();
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
