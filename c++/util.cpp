#include <string>
#include "util.hpp"

void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

void trim(std::string& str) {
    rtrim(str);
    ltrim(str);
}

inline bool ch_eq_case_ins(char a, char b) {
    return(tolower(a) == tolower(b));
}

bool str_eq_case_ins(const std::string& s1, const std::string& s2) {
    return((s1.size() == s2.size()) && equal(s1.begin(), s1.end(), s2.begin(), ch_eq_case_ins));
}