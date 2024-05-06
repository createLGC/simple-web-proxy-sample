#include <iostream>
#include <sstream>
#include <unistd.h>
#include "connection.hpp"

Connection::Connection(int fd): fd(fd) {
    reader = fdopen(fd, "r");
    if(reader == NULL) {
        std::stringstream ss;
        ss << "reader fdopen failed: " << strerror(errno);
        throw ss.str();
    }
    writer = fdopen(fd, "w");
    if(writer == NULL) {
        std::stringstream ss;
        ss << "writer fdopen failed: " << strerror(errno);
        throw ss.str();
    }
}

int Connection::fileno() {
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

void Connection::write(void* ptr, size_t size, size_t nitems) {
    size_t ret = fwrite(ptr, size, nitems, writer);
    if(ret < nitems) {
        if(ferror(writer)) {
            std::stringstream ss;
            ss << "fwrite failed: " << strerror(errno);
            throw ss.str();
        }
    }
}

void Connection::readline(char* buf, int size) {
    char* ret = fgets(buf, size, reader);
    if(ret == NULL) {
        if(ferror(reader)) {
            std::stringstream ss;
            ss << "fgets failed: " << strerror(errno);
            throw ss.str();
        } else if(feof(reader)) {
            throw std::string("connection closed");
        }
    }
}

void Connection::writeline(const char* format, ...) {
    va_list list;
    va_start(list, format);
    int ret = vfprintf(writer, format, list);
    if(ret < 0) {
        std::stringstream ss;
        ss << "vfprintf failed: " << strerror(errno);
        throw ss.str();
    }
    putc('\n', writer);
    va_end(list);
}

void Connection::flush() {
    fflush(writer);
}

void Connection::shutdown() {
    fclose(reader);
    fclose(writer);
    close(fd);
}
