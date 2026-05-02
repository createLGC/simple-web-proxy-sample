#include "util_test.hpp"
#include "url_test.hpp"
#include "http_test.hpp"

extern "C" int test_main() {
    try {

    test_ltrim();
    test_rtrim();
    test_trim();
    test_ch_eq_case_ins();
    test_str_eq_case_ins();

    test_cmp_URL();
    test_URL();

    test_HTTP1Request();

    std::cerr << "test complete!" << std::endl;

    } catch(const std::string& error) {
        std::cerr << error << std::endl;
    }

    return 0;
}