#include "tls.hpp"

#include <sstream>

std::ostream& operator<<(std::ostream& out, const TLSContentType type) {
    switch(type) {
        case TLSContentType::invalid:
            out << "invalid";
            break;
        case TLSContentType::change_cipher_spec:
            out << "change_cipher_spec";
            break;
        case TLSContentType::alert:
            out << "alert";
            break;
        case TLSContentType::handshake:
            out << "handshake";
            break;
        case TLSContentType::application_data:
            out << "application_data";
            break;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const TLSProtocolVersion version) {
    switch(version) {
        case TLSProtocolVersion::v1_0:
            out << "TLS1.0";
            break;
        case TLSProtocolVersion::v1_1:
            out << "TLS1.1";
            break;
        case TLSProtocolVersion::v1_2:
            out << "TLS1.2";
            break;
        case TLSProtocolVersion::v1_3:
            out << "TLS1.3";
            break;
        default:
            out << static_cast<uint16_t>(version);
            break;
    }
    return out;
}

static bool isValidTLSContentType(TLSContentType t) {
    switch(t) {
    case TLSContentType::invalid:
    case TLSContentType::change_cipher_spec:
    case TLSContentType::alert:
    case TLSContentType::handshake:
    case TLSContentType::application_data:
        return true;
    default:
        return false;
    }
}

static bool isValidTLSProtocolVersion(TLSProtocolVersion v) {
    switch(v) {
    case TLSProtocolVersion::v1_0:
    case TLSProtocolVersion::v1_1:
    case TLSProtocolVersion::v1_2:
    case TLSProtocolVersion::v1_3:
        return true;
    default:
        return false;
    }
}

TLSRecord::TLSRecord(Connection& conn) {
    conn.read(&type, sizeof(uint8_t), 1);
    if(!isValidTLSContentType(type)) {
        std::stringstream ss;
        ss << "invalid TLSContentType: " << static_cast<uint8_t>(type);
        throw ss.str();
    }
    
    uint16_t _version;
    conn.read(&_version, sizeof(uint16_t), 1);
    version = static_cast<TLSProtocolVersion>(ntohs(_version));
    if(!isValidTLSProtocolVersion(version)) {
        std::stringstream ss;
        ss << "invalid TLSProtocolVersion: " << static_cast<uint16_t>(version);
        throw ss.str();
    }
    
    conn.read(&length, sizeof(uint16_t), 1);
    length = ntohs(length);
    
    fragment.resize(length);
    conn.read(fragment.data(), 1, length);
}

void TLSRecord::operator>>(Connection& conn) const {
    conn.write(&type, 1, 1);
    uint16_t _version = htons(static_cast<uint16_t>(version));
    conn.write(&_version, 2, 1);
    uint16_t _length = htons(length);
    conn.write(&_length, 2, 1);
    conn.write(fragment.data(), 1, fragment.size());
    conn.flush();
}

void operator<<(Connection& conn, const TLSRecord& record) {
    record >> conn;
}

std::ostream& operator<<(std::ostream& out, const TLSRecord& TLSRecord) {
    out << "type: " << TLSRecord.type << std::endl;
    out << "version: " << TLSRecord.version << std::endl;
    out << "length: " << TLSRecord.length << std::endl;
    out << std::endl;
    return out;
}