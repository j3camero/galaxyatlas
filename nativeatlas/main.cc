#include <iostream>
#include <cassert>
#include <Eigen/Dense>

#include "csv.h"

#include "startree.h"

using namespace startree;
using namespace Eigen;
using namespace std;

const uint32_t kMaxLeafSize = 8;

int main(int argc, char* argv[]) {

    // Read CSV file and create Star objects
    io::CSVReader<8> star_reader("data/hygdata_min3.csv");

    uint64_t id;
    double x, y, z;
    double lum;
    double r, g, b;

    vector<Star> stars;
    while(star_reader.read_row(id, x, y, z, lum, r, g, b)) {
        stars.push_back(Star(id, x, y , z, lum, r, g, b));
    }

    // Put all the stars into the octree
    StarTree tree(kMaxLeafSize, Vector3d::Zero(),
                  Vector3d(-100000.0, -100000.0, -100000.0),
                  Vector3d( 100000.0,  100000.0,  100000.0));

    for (unsigned int i = 0; i < stars.size(); i++) {
        tree.addStar(&(stars[i]));
    }
    
    vector<const Star*> foundStars;
    vector<const StarTree*> searchList;
    searchList.push_back(&tree);
    starsInRadius(Vector3d::Zero(), 2.0, searchList, foundStars);

    cout << foundStars.size() << endl;
    
    return 0;
}
