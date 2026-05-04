#include <iostream>

void test_util();
void test_url();
void test_http();
void test_tls();
void test_server();

extern "C" int test_main() {
    try {

        test_util();

        test_url();

        test_http();

        test_tls();

        test_server();

        std::cerr << "test complete!" << std::endl;

    } catch(const std::string& error) {
        std::cerr << error << std::endl;
    }

    return 0;
}