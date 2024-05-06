CC = clang++
CPPFLAGS = -std=c++17
TARGET = simple-web-proxy-sample
SRCS = main.cpp connection.cpp tls.cpp http.cpp url.cpp util.cpp
OBJS = $(SRCS:.cpp=.o)

$(TARGET): $(OBJS)
	$(CC) -o $@ $^
	-rm -f $(OBJS)

$(OBJS): $(SRCS)
	$(CC) $(CPPFLAGS) -c $(SRCS)

run:
	./$(TARGET)

clean:
	-rm -f $(OBJS) $(TARGET)