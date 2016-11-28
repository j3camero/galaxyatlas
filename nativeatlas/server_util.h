#ifndef SERVER_UTIL_H_
#define SERVER_UTIL_H_

#include <iostream>
#include <cassert>
#include <chrono>
#include <fstream>
#include <memory>

#include <boost/filesystem.hpp>

#include "server_http.hpp"

using namespace std;

typedef SimpleWeb::Server<SimpleWeb::HTTP> HttpServer;

// Send a file (the open ifstream) as a response to an http request
void dflt_res_send(const HttpServer &server,
                   const shared_ptr<HttpServer::Response> &response,
                   const shared_ptr<ifstream> &ifs);

// Default resource request handler
void dflt_res_request_handler(HttpServer& server,
                              shared_ptr<HttpServer::Response> response,
                              shared_ptr<HttpServer::Request> request);

#endif
