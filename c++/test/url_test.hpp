#include "../url.hpp"

#include <cassert>
#include <iostream>

void test_cmp_URL() {
    auto url = URL(
        "scheme",
        "user",
        "password",
        "host",
        "port",
        { "path1", "path2" },
        { { "key1", "value1" }, { "key1", "value2" }, { "key2", "value3"} },
        "anchor"
    );
    assert(url == url);
}

void test_URL() {
    assert(URL("https") == URL("", "", "", "https", "", {}, {}, ""));
    assert(URL("https:") == URL("", "", "", "https", "", {}, {}, ""));
    assert(URL("https://") == URL("https", "", "", "", "", {}, {}, ""));
    assert(URL("https://www.example.com") == URL("https", "", "", "www.example.com", "", {}, {}, ""));
    assert(URL("https://host:") == URL("https", "", "", "host", "", {}, {}, ""));
    assert(URL("https://:@") == URL("https", "", "", "", "", {}, {}, ""));
    assert(URL("https://:@host") == URL("https", "", "", "host", "", {}, {}, ""));
    assert(URL("https://user:@host") == URL("https", "user", "", "host", "", {}, {}, ""));
    assert(URL("https://:password@host") == URL("https", "", "password", "host", "", {}, {}, ""));
    assert(URL("https://user:password@host") == URL("https", "user", "password", "host", "", {}, {}, ""));
    assert(URL("https://:@:") == URL("https", "", "", "", "", {}, {}, ""));
    assert(URL("https://user:password@:port") == URL("https", "user", "password", "", "port", {}, {}, ""));
    assert(URL("https://user:password@host:port") == URL("https", "user", "password", "host", "port", {}, {}, ""));
    assert(URL("/") == URL("", "", "", "", "", {}, {}, ""));
    assert(URL("//") == URL("", "", "", "", "", {}, {}, ""));
    assert(URL("///") == URL("", "", "", "", "", {}, {}, ""));
    assert(URL("https:///") == URL("https", "", "", "", "", {}, {}, ""));
    assert(URL("https://:@:/") == URL("https", "", "", "", "", {}, {}, ""));
    assert(URL("https://user:password@host:port/") == URL("https", "user", "password", "host", "port", {}, {}, ""));
    assert(URL("https://user:password@host:port/あああ") == URL("https", "user", "password", "host", "port", {"あああ"}, {}, ""));
    assert(URL("https://user:password@host:port/あああ/") == URL("https", "user", "password", "host", "port", {"あああ"}, {}, ""));
    assert(URL("https://user:password@host:port/あああ/いいい") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, ""));
    assert(URL("?") == URL("", "", "", "?", "", {}, {}, "")); //不具合だが放置する
    assert(URL("?a=b") == URL("", "", "", "?a=b", "", {}, {}, "")); //不具合だが放置する
    assert(URL("/?") == URL("", "", "", "", "", {}, {}, ""));
    assert(URL("/?a=b") == URL("", "", "", "", "", {}, { {"a", "b"} }, ""));
    assert(URL("///?a=b") == URL("", "", "", "", "", {}, { {"a", "b"} }, ""));
    assert(URL("https://user:password@host:port/あああ/いいい?") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?a=b") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", "b"} }, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?a=b&a=c&d=e") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", "b"}, {"a", "c"}, {"d", "e"} }, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?&a=b&") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", "b"} }, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?a") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", ""} }, ""));
    assert(URL("https://user:password@host:port/あああ/いいい/?a&b") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", ""}, {"b", ""} }, ""));
    assert(URL("#a") == URL("", "", "", "#a", "", {}, {}, "")); //不具合だが放置する
    assert(URL("/#a") == URL("", "", "", "", "", {}, {}, "a"));
    assert(URL("/?a=b#a") == URL("", "", "", "", "", {}, { {"a", "b"} }, "a"));
    assert(URL("https://user:password@host:port/あああ/いいい#a") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, "a"));
    assert(URL("https://user:password@host:port/あああ/いいい/#a") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, {}, "a"));
    assert(URL("https://user:password@host:port/あああ/いいい/?a=b&a=c&d=e#anchor") == URL("https", "user", "password", "host", "port", {"あああ", "いいい"}, { {"a", "b"}, {"a", "c"}, {"d", "e"} }, "anchor"));
}