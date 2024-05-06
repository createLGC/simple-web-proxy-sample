#ifndef util_hpp
#define util_hpp

#include <string>

void ltrim(std::string &s);
void rtrim(std::string &s);
void trim(std::string& str);
bool str_eq_case_ins(const std::string& s1, const std::string& s2);

#endif