CC = clang++

CPPFLAGS = -std=c++17
SRCS = main.cpp connection.cpp tls.cpp http.cpp url.cpp
TARGET = simple-web-proxy-sample

TEST_DIR = ./test
TEST_CPPFLAGS = $(CPPFLAGS) -e _test_main
TEST_SRCS = $(wildcard $(TEST_DIR)/*.cpp) $(SRCS)
TEST_TARGET = simple-web-proxy-sample-test

$(TARGET): $(SRCS)
	$(CC) $(CPPFLAGS) -o $@ $^

run:
	./$(TARGET)

$(TEST_TARGET): $(TEST_SRCS)
	$(CC) $(TEST_CPPFLAGS) -o $(TEST_TARGET) $(TEST_SRCS)

test: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	$(RM) $(OBJS) $(TARGET) $(TEST_TARGET)