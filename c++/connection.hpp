#ifndef connection_hpp
#define connection_hpp

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <vector>
#include <mutex>

class Connection {
private:
    int fd;
    FILE* reader;
    FILE* writer;
    
public:
    Connection() {}
    Connection(int fd);
    
    int fileno();
    
    void read(void* ptr, size_t size, size_t nitems);
    void write(void* ptr, size_t size, size_t nitems);
    
    void readline(char* buf, int size);
    void writeline(const char *format, ...);
    
    void flush();
    void shutdown();
};

#endif