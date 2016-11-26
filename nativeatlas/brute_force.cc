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

const uint32_t kImageWidth = 1280;
const uint32_t kImageHeight = 720;

// Brightness multiplication factor ("exposure time")
const double A = 50000;

const Vector3d cameraPosition(0, -2000, 0);
const Vector3d cameraDirection(0, 1, 0);
const Vector3d upDirection(-sqrt(2), 0, sqrt(2));
const Vector3d rightDirection(cameraDirection.cross(upDirection));

static Vector3d basisProjection(const Vector3d& vec,
                         const Vector3d& basis_a,
                         const Vector3d& basis_b,
                         const Vector3d& basis_c) {
    return Vector3d(vec.dot(basis_a),
                    vec.dot(basis_b),
                    vec.dot(basis_c));
}

int render_count = 0;
int overlap_count = 0;
static void renderStar(Image& image,
                       const uint32_t sx,
                       const uint32_t sy,
                       const double brightness,
                       const Vector3d& color) {
    ColorRGB pxColor = image.pixelColor(sx, sy);
    pxColor.red(1.0);
    pxColor.green(1.0);
    pxColor.blue(1.0);
    double alpha = pxColor.alpha();
    if (alpha != 1.0)
        overlap_count++;
    pxColor.alpha(fmax(alpha - (A*brightness), 0.0));
    image.pixelColor(sx, sy, pxColor);
    
    render_count++;
}

int main(int argc, char* argv[]) {
    //Initialize ImageMagick
    InitializeMagick(*argv);

    // Read CSV file and create Star objects
    //io::CSVReader<8> star_reader("data/hygdata_min3.csv");
    io::CSVReader<8> star_reader("data/thc.csv");
    
    uint64_t id = 0;
    string idstr;
    double x, y, z;
    double absmag;
    double lum;
    double r, g, b;

    double minx,miny,minz,maxx,maxy,maxz;

    vector<Star> stars;
    while(star_reader.read_row(idstr, x, y, z, absmag, r, g, b)) {
        minx = min(x, minx);
        miny = min(y, miny);
        minz = min(z, minz);
        maxx = max(x, maxx);
        maxy = max(y, maxy);
        maxz = max(z, maxz);
        lum = exp(-0.4 * (absmag - 4.85));
        stars.push_back(Star(id++, x, y , z, lum, r, g, b));
    }
    cout << "Min x: " << minx << " Min y: " << miny << " Min z: " << minz
         << endl
         << "Max x: " << maxx << " Max y: " << maxy << " Max z: " << maxz
         << endl;

    // Background image (black)
    Image bgImage(Geometry(kImageWidth, kImageHeight), Color("black"));
    
    // Create clear image for stars
    Image starImage(Geometry(kImageWidth, kImageHeight), Color("black"));
    starImage.matte(true);
    starImage.transparent(Color("black"));
    assert(((ColorRGB)(starImage.pixelColor(0,0))).red() == 0.0);
    assert(((ColorRGB)(starImage.pixelColor(0,0))).green() == 0.0);
    assert(((ColorRGB)(starImage.pixelColor(0,0))).blue() == 0.0);
    assert(starImage.pixelColor(0,0).alpha() == 1.0);
    
    for (vector<Star>::iterator star = stars.begin();
         star != stars.end(); ++star) {
        const Vector3d& position = star->position();
        const Vector3d translated = position - cameraPosition;
        const Vector3d projected = basisProjection(translated,
                                                   rightDirection,
                                                   cameraDirection,
                                                   upDirection);
        if (projected.y() < 0.00001) {
            continue;
        }
        const uint32_t sx = (kImageWidth / 2.0) +
            (kImageWidth / 2.0) * projected.x() / projected.y();
        const uint32_t sy = (kImageHeight / 2.0) +
            (kImageWidth / 2.0) * projected.z() / projected.y();
        if (sx < 0 || sx >= kImageWidth) continue;
        if (sy < 0 || sy >= kImageHeight) continue;
        const double brightness = star->lum() /
            translated.squaredNorm();
        renderStar(starImage, sx, sy, brightness, star->color());
    }

    bgImage.composite(starImage, 0, 0, PlusCompositeOp);

    cout << render_count << endl;
    cout << overlap_count << endl;
    
    bgImage.write("image.png");
    
    return 0;
}
