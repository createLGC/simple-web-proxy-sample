#ifndef connection_hpp
#define connection_hpp

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <vector>
#include <mutex>

class Connection {
private:
    const int fd;
    FILE* const reader;
    FILE* const writer;
    
public:
    Connection(int fd);
    
    int fileno() const;
    
    void read(void* ptr, size_t size, size_t nitems);
    void write(const void* ptr, size_t size, size_t nitems);
    
    void readline(char* buf, int size);
    void writeline(const char *format, ...);
    
    void flush();
    void shutdown();
};

#endif