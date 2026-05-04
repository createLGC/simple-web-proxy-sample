#ifndef connection_hpp
#define connection_hpp

#include <cstdio>
#include <string>

class Connection {
private:
    const int fd;
    FILE* const reader;
    FILE* const writer;
    
public:
    Connection(int fd);
    ~Connection();
    
    int fileno() const;
    
    void read(void* ptr, size_t size, size_t nitems); //size分読み込めなかった場合は例外をthrow
    void write(const void* ptr, size_t size, size_t nitems);
    void write(const char *format, ...);

    std::string readline(); //改行が読み込めなかった場合は例外をthrow
    
    void flush();
    void shutdown();
};

#endif