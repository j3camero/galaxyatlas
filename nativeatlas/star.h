#ifndef STAR_H_
#define STAR_H_

#include <Eigen/Dense>

using Eigen::Vector3d;

namespace startree {
class Star {
public:
    Star(uint64_t id, double x, double y, double z, double lum,
         double r, double g, double b) :
        id_(id), position_(x, y, z), lum_(lum),
        color_(r, g, b) {}

    uint64_t id() const { return id_; }
    const Vector3d& position() const { return position_; }
    double lum() const { return lum_; }
    const Vector3d& color() const { return color_; }

private:
    uint64_t id_;
    Vector3d position_;
    double lum_;
    Vector3d color_;
};
} // namespace StarTree

#endif // STAR_H_
