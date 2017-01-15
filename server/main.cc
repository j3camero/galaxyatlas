#include <iostream>
#include <cassert>
#include <chrono>
#include <fstream>
#include <algorithm>
#include <signal.h>

#include <Eigen/Dense>

#include "csv.h"
#include "server_http.hpp"

#include "server_util.h"
#include "startree.h"

using namespace startree;
using namespace Eigen;
using namespace std;
using namespace std::chrono;

const uint32_t kMaxLeafSize = 8;
//const string kDataFileName = "data/hygdata_min3.csv";
const string kDataFileName = "data/thc.csv";

// Signal handler sets this to false to stop the server
volatile bool running = true;
void signal_handler(int signum) {
    if (signum == SIGINT ||
        signum == SIGTERM ||
        signum == SIGHUP) {
        running = false;
    }
}

int main(int argc, char* argv[]) {
    // Setup signal handler
    signal(SIGINT, signal_handler);
    signal(SIGHUP, signal_handler);
    signal(SIGTERM, signal_handler);
    
    // Read CSV file and create Star objects
    cout << "Reading CSV file..." << endl;
    io::CSVReader<8> star_reader(kDataFileName);
    
    uint64_t id = 0;
    string idstr;
    double x, y, z;
    double absmag;
    double lum;
    double r, g, b;

    double minx,miny,minz,maxx,maxy,maxz;

    vector<Star> stars;
    while(star_reader.read_row(idstr, x, y, z, absmag, r, g, b)) {
    //while(star_reader.read_row(id, x, y, z, lum, r, g, b)) {
        minx = min(x, minx);
        miny = min(y, miny);
        minz = min(z, minz);
        maxx = max(x, maxx);
        maxy = max(y, maxy);
        maxz = max(z, maxz);
        lum = exp(-0.4 * (absmag - 4.85));
        stars.push_back(Star(id++, x, y , z, lum, r, g, b));
        //stars.push_back(Star(id, x, y , z, lum, r, g, b));
    }
    cout << "Min x: " << minx << " Min y: " << miny << " Min z: " << minz
         << endl
         << "Max x: " << maxx << " Max y: " << maxy << " Max z: " << maxz
         << endl;
    cout << "Done reading CSV file." << endl;

    cout << "Loading stars into tree..." << endl;
    high_resolution_clock::time_point t1 = high_resolution_clock::now();

    // Calculate bounding box
    double maxmax = fmax(maxx, maxy);
    maxmax = fmax(maxmax, maxz);
    Vector3d maxVec(maxmax + 1.0, maxmax + 1.0, maxmax + 1.0);

    double minmin = fmin(minx, miny);
    minmin = fmin(minmin, minz);
    Vector3d minVec(minmin - 1.0, minmin - 1.0, minmin - 1.0);

    if (maxVec.norm() > minVec.norm()) {
        minVec = -1 * maxVec;
    } else {
        maxVec = -1 * minVec;
    }
    
    // Put all the stars into the octree
    StarTree tree(kMaxLeafSize, Vector3d::Zero(),
                  minVec,
                  maxVec);
    map<uint64_t,const StarTree*> treeMap;
    treeMap[0] = &tree;

    for (unsigned int i = 0; i < stars.size(); i++) {
        tree.addStar(&(stars[i]), treeMap);
    }
    high_resolution_clock::time_point t2 = high_resolution_clock::now();
    duration<double> time_span =
        duration_cast<duration<double>>(t2 - t1);
    cout << "Done loading stars into tree (" << time_span.count()
         <<"s)." << endl;
    cout << "Number of nodes: " << treeMap.size() << endl;

    // Create web server
    cout << "Staring web server..." << endl;
    HttpServer server(8080, 1);

    // starsInRadius API
    server.resource["^/starsInRadius[?]"
                    "((radius=[^=&]*)|"
                    "(pointX=[^=&]*)|"
                    "(pointY=[^=&]*)|"
                    "(pointZ=[^=&]*)|"
                    "&)*"]["GET"] =
        [&server,&tree](shared_ptr<HttpServer::Response> response,
                        shared_ptr<HttpServer::Request> request) {
        starsInRadiusHandler(server, response, request, tree);
    };

    // visibleStars API
    server.resource["^/visibleStars[?]"
                    "((minLum=[^=&]*)|"
                    "(pointX=[^=&]*)|"
                    "(pointY=[^=&]*)|"
                    "(pointZ=[^=&]*)|"
                    "&)*"]["GET"] =
        [&server,&tree](shared_ptr<HttpServer::Response> response,
                        shared_ptr<HttpServer::Request> request) {
        visibleStarsHandler(server, response, request, tree);
    };

    // visibleStarsMagic API
    server.resource["^/visibleStarsMagic[?]"
                    "((minLum=[^=&]*)|"
                    "(blurRad=[^=&]*)|"
                    "(pointX=[^=&]*)|"
                    "(pointY=[^=&]*)|"
                    "(pointZ=[^=&]*)|"
                    "&)*"]["GET"] =
        [&server,&tree](shared_ptr<HttpServer::Response> response,
                        shared_ptr<HttpServer::Request> request) {
        visibleStarsMagicHandler(server, response, request, tree);
    };

    // visibleOctants API
    server.resource["^/visibleOctants[?]"
                    "((minLum=[^=&]*)|"
                    "(pointX=[^=&]*)|"
                    "(pointY=[^=&]*)|"
                    "(pointZ=[^=&]*)|"
                    "&)*"]["GET"] =
        [&server,&tree](shared_ptr<HttpServer::Response> response,
                        shared_ptr<HttpServer::Request> request) {
        visibleOctantsHandler(server, response, request, tree);
    };
    
    // visibleOctantsMagic API
    server.resource["^/visibleOctantsMagic[?]"
                    "((minLum=[^=&]*)|"
                    "(blurRad=[^=&]*)|"
                    "(pointX=[^=&]*)|"
                    "(pointY=[^=&]*)|"
                    "(pointZ=[^=&]*)|"
                    "&)*"]["GET"] =
        [&server,&tree](shared_ptr<HttpServer::Response> response,
                        shared_ptr<HttpServer::Request> request) {
        visibleOctantsMagicHandler(server, response, request, tree);
    };

    // getNodeStars API
    server.resource["/getNodeStars"]["POST"] =
        [&server,&treeMap](shared_ptr<HttpServer::Response> response,
                           shared_ptr<HttpServer::Request> request) {
        getNodeStarsHandler(server, response, request, treeMap);
    };
    
    // Serve the static pages
    server.default_resource["GET"] =
        [&server](shared_ptr<HttpServer::Response> response,
                  shared_ptr<HttpServer::Request> request) {
        dflt_res_request_handler(server, response, request);
    };

    // Start the webserver on another thread
    thread server_thread([&server]() {
            server.start();
        });
    cout << "Web server started." << endl;

    // This thread should just sleep while we're running.
    while (running) {
        this_thread::sleep_for(seconds(1));
    }

    // Stop the webserver
    cout << "Web server shutting down." << endl;
    server.stop();
    // Join the webserver thread
    server_thread.join();
    
    // Run a search for visible stars
    /*
    cout << "Searching for visible stars..." << endl;
    t1 = high_resolution_clock::now();
    vector<const StarTree*> searchList{&tree};
    vector<const Star*> foundStars;
    visibleStars(cameraPosition, 0.1, searchList, foundStars);
    t2 = high_resolution_clock::now();
    time_span = duration_cast<duration<double>>(t2 - t1);
    cout << "Found " << foundStars.size() << " visible stars ("
         << time_span.count() << "s)." << endl;
    */
    
    return 0;
}
