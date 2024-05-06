#ifndef tls_hpp
#define tls_hpp

#include <stdio.h>
#include <stdint.h>
#include <iostream>
#include <vector>
#include <list>

#include "connection.hpp"

enum class TLSContentType: uint8_t {
    invalid = 0,
    change_cipher_spec = 20,
    alert = 21,
    handshake = 22,
    application_data = 23
};

std::ostream& operator<<(std::ostream& out, const TLSContentType type);

enum class TLSProtocolVersion: uint16_t {
    v1_0 = 0x0301,
    v1_1 = 0x0302,
    v1_2 = 0x0303,
    v1_3 = 0x0304
};

std::ostream& operator<<(std::ostream& out, const TLSProtocolVersion version);

class TLSRecord {
public:
    TLSContentType type;
    TLSProtocolVersion version;
    uint16_t length;
    std::vector<uint8_t> fragment;
    
    TLSRecord(Connection& conn);
    void write(Connection& conn);
};

std::ostream& operator<<(std::ostream& out, const TLSRecord& record);

#endif
