#include "FILEObj.hpp"
#include "../tls.hpp"

#include <cassert>

static void test_TLSRecord() {
    const char record_data[] = { 21, 0x03, 0x03, 0, 2, 0, 0 };

    auto f = FILEObj(record_data, sizeof(record_data));

    auto conn = Connection(f.descriptor());
    
    auto record = TLSRecord(conn);

    assert(record.type == TLSContentType::alert && record.version == TLSProtocolVersion::v1_2 && record.length == 2 && record.fragment == std::vector<uint8_t>(2, 0));
}

static void test_invalid_TLSContentType() {
    const char record_data[] = { 100, 0x03, 0x03, 0, 2, 0, 0 };

    auto f = FILEObj(record_data, sizeof(record_data));

    auto conn = Connection(f.descriptor());
    
    try {
        auto record = TLSRecord(conn);
        assert(false);
    } catch(const std::string& error) {
        constexpr char error_text[] = "invalid TLSContentType";
        assert(std::equal(error_text, error_text + sizeof(error_text) - 1, std::begin(error)));
    } catch(...) {
        assert(false);
    }
}

static void test_invalid_TLSProtocolVersion() {
    const char record_data[] = { 21, 0x04, 0x03, 0, 2, 0, 0 };

    auto f = FILEObj(record_data, sizeof(record_data));

    auto conn = Connection(f.descriptor());
    
    try {
        auto record = TLSRecord(conn);
        assert(false);
    } catch(const std::string& error) {
        constexpr char error_text[] = "invalid TLSProtocolVersion";
        assert(std::equal(error_text, error_text + sizeof(error_text) - 1, std::begin(error)));
    } catch(...) {
        assert(false);
    }
}

static void test_too_long_length() {
    const char record_data[] = { 21, 0x03, 0x03, 0, 3, 0, 0 };

    auto f = FILEObj(record_data, sizeof(record_data));

    auto conn = Connection(f.descriptor());
    
    try {
        auto record = TLSRecord(conn);
        assert(false);
    } catch(const std::string& error) {
        assert(error == "connection closed");
    } catch(...) {
        assert(false);
    }
}

void test_tls() {
    std::cerr << "****** start test_tls ******" << std::endl;
    test_TLSRecord();
    test_invalid_TLSContentType();
    test_invalid_TLSProtocolVersion();
    test_too_long_length();
    std::cerr << "****** end test_tls ******" << std::endl;
}