#ifndef connection_hpp
#define connection_hpp

#include <cstdio>

class Connection {
private:
    const int fd;
    FILE* const reader;
    FILE* const writer;
    
public:
    Connection(int fd);
    ~Connection();
    
    int fileno() const;
    
    void read(void* ptr, size_t size, size_t nitems);
    void write(const void* ptr, size_t size, size_t nitems);
    
    void readline(char* buf, int size);
    void writeline(const char *format, ...);
    
    void flush();
    void shutdown();
};

#endif