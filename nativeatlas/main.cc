#include <iostream>
#include <cassert>
#include <Eigen/Dense>
#include <Magick++.h>

#include "csv.h"

#include "startree.h"

using namespace startree;
using namespace Eigen;
using namespace Magick;
using namespace std;

const uint32_t kMaxLeafSize = 8;

Vector3d basisProjection(const Vector3d& vec,
                         const Vector3d& basis_a,
                         const Vector3d& basis_b,
                         const Vector3d& basis_c) {
    return Vector3d(vec.dot(basis_a),
                    vec.dot(basis_b),
                    vec.dot(basis_c));
}

int main(int argc, char* argv[]) {
    //Initialize ImageMagick
    InitializeMagick(*argv);

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

    Vector3d cameraPosition(0, 0, 0);
    Vector3d cameraDirection(0, 1, 0);
    Vector3d upDirection(0, 0, 1);
    Vector3d right = cameraDirection.cross(upDirection);

    // Run a search for visible stars
    vector<const StarTree*> searchList{&tree};
    vector<const Star*> foundStars;
    visibleStars(cameraPosition, 0.01, searchList, foundStars);

    // Create an image from the stars
    Image image("1280x720", "black");
    for (vector<const Star*>::iterator it = foundStars.begin();
         it != foundStars.end(); ++it) {
        const Vector3d& position = (*it)->position();
        const Vector3d translated = position - cameraPosition;
        const Vector3d projected = basisProjection(translated,
                                                   right,
                                                   cameraDirection,
                                                   upDirection);
        
    }
    
    cout << foundStars.size() << endl;
    
    return 0;
}
