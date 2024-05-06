#include "tls.hpp"

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

TLSRecord::TLSRecord(Connection& conn) {
    conn.read(&type, 1, 1);
    
    uint16_t _version;
    conn.read(&_version, 2, 1);
    version = static_cast<TLSProtocolVersion>(ntohs(_version));
    
    conn.read(&length, 2, 1);
    length = ntohs(length);
    
    uint8_t buf[length];
    conn.read(buf, 1, length);
    fragment = std::vector<uint8_t>(buf, buf + length);
}

void TLSRecord::write(Connection& conn) {
    conn.write(&type, 1, 1);
    uint16_t _version = htons(static_cast<uint16_t>(version));
    conn.write(&_version, 2, 1);
    uint16_t _length = htons(length);
    conn.write(&_length, 2, 1);
    conn.write(fragment.data(), 1, fragment.size());
    conn.flush();
}

std::ostream& operator<<(std::ostream& out, const TLSRecord& TLSRecord) {
    out << "type: " << TLSRecord.type << std::endl;
    out << "version: " << TLSRecord.version << std::endl;
    out << "length: " << TLSRecord.length << std::endl;
    out << std::endl;
    return out;
}