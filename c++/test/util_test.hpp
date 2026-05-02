#include <cassert>
#include <iostream>

#include "../util.hpp"

void test_ltrim() {
    std::string str(" \n\r　aiueo　\n\r ");

    ltrim(str);
    assert(str == "　aiueo　\n\r ");
}

void test_rtrim() {
    std::string str(" \n\r　aiueo　\n\r ");

    rtrim(str);
    assert(str == " \n\r　aiueo　");
}

void test_trim() {
    std::string str(" \n\r　aiueo　\n\r ");

    trim(str);
    assert(str == "　aiueo　");
}

void test_ch_eq_case_ins() {
    assert(ch_eq_case_ins('a', 'A'));
}

void test_str_eq_case_ins() {
    std::string s1("abcde"), s2("ABCDE");
    assert(str_eq_case_ins(s1, s2));
}