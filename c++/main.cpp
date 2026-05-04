#include "server.hpp"
#include <iostream>

int main(int argc, const char * argv[]) {
    try {
        int port_number;
        switch(argc) {
            case 1:
                port_number = 8080;
                break;
            case 2:
                port_number = atoi(argv[1]);
                if(port_number == 0) {
                    throw std::string("An invalid port number. Please pass an integer for the port number.");
                }
                break;
            default:
                throw std::string("Too many arguments. This program accepts only 1 argument. That is the port number which this server listens.");
        }
        start_server(port_number);
    } catch(std::string& error) {
        std::cerr << "FATAL " << error << std::endl;
        return 1;
    }
    
    return 0;
}
