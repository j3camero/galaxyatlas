#include <iostream>
#include <cassert>
#include <chrono>
#include <fstream>
#include <memory>
#include <cstring>

#include <boost/filesystem.hpp>
#include <json/json.h>

#include "server_util.h"

using namespace std;
using namespace startree;

void starsInRadiusHandler(HttpServer& server,
                          shared_ptr<HttpServer::Response> response,
                          shared_ptr<HttpServer::Request> request,
                          const StarTree& tree) {
    double radius, pointX, pointY, pointZ;
    string content;
    bool error = false;
    string errorStr;

    string valstring;
    for (int i = 2; i < 6; i++) {
        string pair = request->path_match[i];
        int splitPoint = pair.find("=");
        string fieldname = pair.substr(0, splitPoint);
        valstring = pair.substr(splitPoint + 1);

        if (fieldname == "radius") {
            radius = atof(valstring.c_str());
        } else if (fieldname == "pointX") {
            pointX = atof(valstring.c_str());
        } else if (fieldname == "pointY") {
            pointY = atof(valstring.c_str());
        } else if (fieldname == "pointZ") {
            pointZ = atof(valstring.c_str());
        }
    }

    vector<const StarTree*> searchList{&tree};
    vector<const Star*> foundStars;
    starsInRadius(Vector3d(pointX, pointY, pointZ), radius,
                  searchList, foundStars);
    
    content = "Stars found: " + to_string(foundStars.size());
    *response << "HTTP/1.1 200 OK\r\n"
              << "Content-Length: " << content.length()
              << "\r\n\r\n" << content;
}

void dflt_res_send(const HttpServer &server,
                   const shared_ptr<HttpServer::Response> &response,
                   const shared_ptr<ifstream> &ifs) {
    //read and send 128 KB at a time
    static vector<char> buffer(131072); // Safe when server is running on one thread
    streamsize read_length;
    
    if((read_length =
        ifs->read(&buffer[0], buffer.size()).gcount()) > 0) {
        
        response->write(&buffer[0], read_length);
        if(read_length == static_cast<streamsize>(buffer.size())) {
            server.send(response,
                        [&server,
                         response,
                         ifs](const boost::system::error_code &ec) {
                            if(!ec)
                                dflt_res_send(server, response,
                                              ifs);
                            else
                                cerr << "Connection interrupted" << endl;
                        });
        }
    }
}

void dflt_res_request_handler(HttpServer& server,
                              shared_ptr<HttpServer::Response> response,
                              shared_ptr<HttpServer::Request> request) {
    auto web_root_path = boost::filesystem::canonical("static");
    boost::system::error_code ec;
    auto path =  boost::filesystem::canonical(web_root_path /
                                              request->path,
                                              ec);
    if (ec != boost::system::errc::success) {
        string content = "Could not open path " +
            request->path + ", file does not exist.";
        *response << "HTTP/1.1 400 Bad Request\r\nContent-Length: "
                  << content.length() << "\r\n\r\n" << content;
        return;
    }
    
    //Check if path is within web_root_path
    if(distance(web_root_path.begin(), web_root_path.end()) >
       distance(path.begin(), path.end()) ||
       !equal(web_root_path.begin(), web_root_path.end(),
              path.begin())) {
        string content = "Could not open path " +
            request->path + ", path must be within web root.";
        *response << "HTTP/1.1 400 Bad Request\r\nContent-Length: "
                  << content.length() << "\r\n\r\n" << content;
        return;
    }
    // Check if they requested a directory
    if(boost::filesystem::is_directory(path))
        path/="index.html";
    
    // Check if the file is a normal file
    if(!boost::filesystem::is_regular_file(path)) {
        string content = "Could not open path " +
            request->path + ", file does not exist.";
        *response << "HTTP/1.1 400 Bad Request\r\nContent-Length: "
                  << content.length() << "\r\n\r\n" << content;
        return;
    }

    auto ifs = make_shared<ifstream>();
    ifs->open(path.string(), ifstream::in | ios::binary);
    if (*ifs) {
        ifs->seekg(0, ios::end);
        auto length = ifs->tellg();

        ifs->seekg(0, ios::beg);

        *response << "HTTP/1.1 200 OK\r\nContent-Length: "
                  << length << "\r\n\r\n";
        dflt_res_send(server, response, ifs);
    } else {
        string content = "Could not open path " +
            request->path + ", could not read file.";
        *response << "HTTP/1.1 400 Bad Request\r\nContent-Length: "
                  << content.length() << "\r\n\r\n" << content;
        return;
    }
}
