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

const uint32_t kImageWidth = 800;
const uint32_t kImageHeight = 600;

// Brightness multiplication factor ("exposure time")
//const double A = 30000;
const double A = 10000;

const float numSeconds = 4.0;
const uint32_t fps = 1;
const double frame_period_s = 1 / fps;
const uint32_t numFrames = numSeconds * fps;
const double radsPerFrame = (2 * M_PI) / numFrames;

Vector3d cameraPosition(0, -1, 0);
Vector3d cameraDirection(0, 1, 0);
const Vector3d upDirection(0, 0, 1);
Vector3d rightDirection(cameraDirection.cross(upDirection));

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

    cout << "Seconds: " << numSeconds << endl;
    cout << "FPS: " << fps << endl;
    cout << "Period: " << frame_period_s << endl;
    cout << "Frames: " << numFrames << endl;
    cout << "rads/frame: " << radsPerFrame << endl;
    
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

    vector<Image> frameImages;
    Matrix3d rotM =
        AngleAxisd(radsPerFrame, upDirection).toRotationMatrix();
    for (unsigned int i = 0; i < numFrames; i++) {
        cout << "Starting frame: " << i << endl;
        
        // Background image (black)
        frameImages.push_back(Image(Geometry(kImageWidth, kImageHeight),
                                    Color("black")));
    
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

        frameImages.back().composite(starImage, 0, 0, PlusCompositeOp);
        frameImages.back().animationDelay(frame_period_s * 100);

        // Rotate camera position about up axis through origin
        Vector3d tmp = rotM * cameraPosition;
        cameraPosition = tmp;
        // Set camera direction towards the origin
        tmp = cameraPosition;
        cameraDirection = -(tmp.normalized());
        rightDirection = cameraDirection.cross(upDirection);

        cout << "New camera position: " << cameraPosition << endl;
        cout << "New camera direction: " << cameraDirection << endl;
    }
    
    writeImages(frameImages.begin(), frameImages.end(), "animation.gif");
    
    return 0;
}
