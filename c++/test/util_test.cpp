#include <cassert>
#include <iostream>

#include "../util.hpp"

static void test_ltrim() {
    std::string str(" \n\r　aiueo　\n\r ");

    ltrim(str);
    assert(str == "　aiueo　\n\r ");
}

static void test_rtrim() {
    std::string str(" \n\r　aiueo　\n\r ");

    rtrim(str);
    assert(str == " \n\r　aiueo　");
}

static void test_trim() {
    std::string str(" \n\r　aiueo　\n\r ");

    trim(str);
    assert(str == "　aiueo　");
}

void test_util() {
    std::cerr << "****** start test_util ******" << std::endl;
    test_ltrim();
    test_rtrim();
    test_trim();
    std::cerr << "****** end test_util ******" << std::endl;
}