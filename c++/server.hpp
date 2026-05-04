#ifndef server_hpp
#define server_hpp

#include <string>
#include "connection.hpp"
#include "http.hpp"

uint32_t resolve_host(const std::string& host);
int connect_to_server(const std::string& host, int port);
void tls_relay(Connection& src, Connection& dst);
void http_relay(Connection& client, Connection& server, HTTP1Request* request);
HTTP1Response create_error_response(std::string error);
void start_relay(int client_fd);
void start_server(int port);

#endif