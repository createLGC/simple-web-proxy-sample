#include "connection.hpp"

#include <cassert>
#include <cstdarg>
#include <iostream>
#include <sstream>
#include <unistd.h>

Connection::Connection(int fd): fd(fd), reader(fdopen(fd, "r")), writer(fdopen(fd, "w")) {
    if(reader == NULL) {
        std::stringstream ss;
        ss << "reader fdopen failed: " << strerror(errno);
        throw ss.str();
    }
    if(writer == NULL) {
        std::stringstream ss;
        ss << "writer fdopen failed: " << strerror(errno);
        throw ss.str();
    }
}

Connection::~Connection() {
    shutdown();
}

int Connection::fileno() const {
    return fd;
}

void Connection::read(void* ptr, size_t size, size_t nitems) {
    size_t ret = fread(ptr, size, nitems, reader);
    if(ret < nitems) {
        if(ferror(reader)) {
            std::stringstream ss;
            ss << "fread failed: " << strerror(errno);
            throw ss.str();
        } else if(feof(reader)) {
            throw std::string("connection closed");
        }
    }
}

void Connection::write(const void* ptr, size_t size, size_t nitems) {
    size_t ret = fwrite(ptr, size, nitems, writer);
    if(ret < nitems) {
        if(ferror(writer)) {
            std::stringstream ss;
            ss << "fwrite failed: " << strerror(errno);
            throw ss.str();
        }
    }
}

void Connection::write(const char* format, ...) {
    va_list list;
    va_start(list, format);
    int ret = vfprintf(writer, format, list);
    if(ret < 0) {
        std::stringstream ss;
        ss << "vfprintf failed: " << strerror(errno);
        throw ss.str();
    }
    va_end(list);
}

std::string Connection::readline() {
    std::string buf;
    constexpr size_t BUF_SIZE = 1024;
    while(true) {
        std::array<char, BUF_SIZE> _buf{};
        if(fgets(_buf.data(), BUF_SIZE, reader) == nullptr) {
            if(ferror(reader)) {
                std::stringstream ss;
                ss << "fgets failed: " << strerror(errno);
                throw ss.str();
            } else if(feof(reader)) {
                throw std::string("connection closed");
            } else {
                assert(false);
            }
        }
        size_t len = strlen(_buf.data());
        buf += std::string(_buf.data(), len);
        if(_buf[len - 1] == '\n') break;
    }
    return buf;
}

void Connection::flush() {
    fflush(writer);
}

void Connection::shutdown() {
    fclose(reader);
    fclose(writer);
    close(fd);
}
