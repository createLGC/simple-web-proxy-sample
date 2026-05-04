#ifndef FILEObj_hpp
#define FILEObj_hpp

#include <cstdio>

class FILEObj {
private:
    FILE* fp;

    FILEObj(FILE* fp): fp(fp) {}

public:
    FILEObj(): FILEObj(tmpfile()) {}
    FILEObj(const void* ptr, size_t size): FILEObj() {
        fwrite(ptr, 1, size, fp);
        fflush(fp);
        seekToHead();
    }

    ~FILEObj() { close(); }

    FILEObj(const FILEObj&) = delete;
    FILEObj& operator=(const FILEObj&) = delete;

    FILEObj(FILEObj&&) = default;
    FILEObj& operator=(FILEObj&&) = default;

    int descriptor() const { return fileno(fp); }

    bool eof() const { return feof(fp) != 0; }

    void seekToHead() { fseek(fp, 0, SEEK_SET); }

    void close() { fclose(fp); }
};

#endif