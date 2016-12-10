#ifndef SERVER_UTIL_H_
#define SERVER_UTIL_H_

#include "server_http.hpp"

#include "startree.h"

using namespace std;
using namespace startree;

typedef SimpleWeb::Server<SimpleWeb::HTTP> HttpServer;

// Handler for starsInRadius call
void starsInRadiusHandler(HttpServer& server,
                          shared_ptr<HttpServer::Response> response,
                          shared_ptr<HttpServer::Request> request,
                          const StarTree& tree);

// Handler for starsInRadius call
void visibleStarsHandler(HttpServer& server,
                         shared_ptr<HttpServer::Response> response,
                         shared_ptr<HttpServer::Request> request,
                         const StarTree& tree);

// Handler for starsInRadius call
void visibleStarsMagicHandler(HttpServer& server,
                              shared_ptr<HttpServer::Response> response,
                              shared_ptr<HttpServer::Request> request,
                              const StarTree& tree);

// Send a file (the open ifstream) as a response to an http request
void dflt_res_send(const HttpServer &server,
                   const shared_ptr<HttpServer::Response> &response,
                   const shared_ptr<ifstream> &ifs);

// Default resource request handler
void dflt_res_request_handler(HttpServer& server,
                              shared_ptr<HttpServer::Response> response,
                              shared_ptr<HttpServer::Request> request);

#endif
