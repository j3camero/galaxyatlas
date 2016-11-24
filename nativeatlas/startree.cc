#include <cmath>
#include <vector>
#include <cassert>

#include <Eigen/Dense>

#include "startree.h"

namespace startree {
// Make a have the minimum x, y and z coordinates of a and b
static void minVec(Vector3d& a, const Vector3d& b) {
    a[X] = fmin(a[X], b[X]);
    a[Y] = fmin(a[Y], b[Y]);
    a[Z] = fmin(a[Z], b[Z]);
}

// Make a have the maximum x, y and z coordinates of a and b
static void maxVec(Vector3d& a, const Vector3d& b) {
    a[X] = fmax(a[X], b[X]);
    a[Y] = fmax(a[Y], b[Y]);
    a[Z] = fmax(a[Z], b[Z]);
}

/* Return new set of sub-bounds given the parent bounds, the split point
 * and the direction (which octant) */
static void
splitBounds(const Vector3d oldBounds[2], Vector3d newBounds[2],
            const Vector3d splitPoint,
            TreeDirection dir) {
    switch(dir) {
    case NED:
        newBounds[0] = splitPoint;
        newBounds[1] = oldBounds[1];
        break;
    case NWD:
        newBounds[0] = splitPoint;
        newBounds[0][Y] = oldBounds[0][Y];
        newBounds[1] = oldBounds[1];
        newBounds[1][Y] = splitPoint[Y];
        break;
    case SED:
        newBounds[0] = splitPoint;
        newBounds[0][X] = oldBounds[0][X];
        newBounds[1] = oldBounds[1];
        newBounds[1][X] = splitPoint[X];
        break;
    case SWD:
        newBounds[0] = oldBounds[0];
        newBounds[0][Z] = splitPoint[Z];
        newBounds[1] = splitPoint;
        newBounds[1][Z] = oldBounds[1][Z];
        break;
    case NEU:
        newBounds[0] = splitPoint;
        newBounds[0][Z] = oldBounds[0][Z];
        newBounds[1] = oldBounds[1];
        newBounds[1][Z] = splitPoint[Z];
        break;
    case NWU:
        newBounds[0] = oldBounds[0];
        newBounds[0][X] = splitPoint[X];
        newBounds[1] = splitPoint;
        newBounds[1][X] = oldBounds[1][X];
        break;
    case SEU:
        newBounds[0] = oldBounds[0];
        newBounds[0][Y] = splitPoint[Y];
        newBounds[1] = splitPoint;
        newBounds[1][Y] = oldBounds[1][Y];
        break;
    case SWU:
        newBounds[0] = oldBounds[0];
        newBounds[1] = splitPoint;
        break;
    }
}

StarTree::StarTree(uint64_t maxLeafSize, Vector3d splitPoint,
                   Vector3d minBounds, Vector3d maxBounds) :
    isLeaf_(true),
    splitPoint_(splitPoint),
    branches_{nullptr, nullptr, nullptr, nullptr,
              nullptr, nullptr, nullptr, nullptr},

    isRoot_(true),
    parent_(nullptr),
    maxLeafSize_(maxLeafSize),
    numStars_(0),
    maxLuminosity_(0),
    sumLuminosity_(0),
    sumColor_(0, 0, 0),
    averageColor_(0, 0, 0),
    treeBounds_{minBounds, maxBounds},
    starBounds_{Vector3d::Zero(), Vector3d::Zero()},
    boundsCenter_(0, 0, 0),
    boundsRadius_(0) {}

StarTree::StarTree(uint64_t maxLeafSize, Vector3d splitPoint,
                   StarTree* parent) :
    isLeaf_(true),
    splitPoint_(splitPoint),
    branches_{nullptr, nullptr, nullptr, nullptr,
        nullptr, nullptr, nullptr, nullptr},
    isRoot_(false),
    parent_(parent),
    maxLeafSize_(maxLeafSize),
    numStars_(0),
    maxLuminosity_(0),
    sumLuminosity_(0),
    sumColor_(0, 0, 0),
    averageColor_(0, 0, 0),
    treeBounds_{Vector3d::Zero(), Vector3d::Zero()},
    starBounds_{Vector3d::Zero(), Vector3d::Zero()},
    boundsCenter_(0, 0, 0),
    boundsRadius_(0) {}

// This employs a trick similar to the chmod permission bits
TreeDirection
StarTree::getDirection(const Vector3d& from, const Vector3d& to) {
    bool cx = from[X] >= to[X];
    bool cy = from[Y] >= to[Y];
    bool cz = from[Z] >= to[Z];
    int direction =
        (cx ? 1 : 0) +
        (cy ? 2 : 0) +
        (cz ? 4 : 0);
    return static_cast<TreeDirection>(direction);
}

void
StarTree::addStarMetadata(const Star* star) {
    numStars_ += 1;

    maxLuminosity_ =
        star->lum() > maxLuminosity_ ? star->lum() : maxLuminosity_;
    sumLuminosity_ += star->lum();

    sumColor_ += star->color();
    averageColor_ = sumColor_ / numStars_;

    minVec(starBounds_[0], star->position());
    maxVec(starBounds_[1], star->position());

    boundsCenter_ = 0.5 * (treeBounds_[0] + treeBounds_[1]);
    boundsRadius_ = (boundsCenter_ - treeBounds_[1]).norm();
}

void
StarTree::addStar(const Star* star) {
    TreeDirection dir;
    // Always add the metadata
    addStarMetadata(star);

    // Do things differently depending on what type of node we are
    if (isLeaf_) {
        // If we'd go over our maximum leaf size, we need to split up
        if (numStars_ > maxLeafSize_) {
            isLeaf_ = false;
            // Create the empty child nodes
            for (int i = 0; i < 8; i++) {
                assert(branches_[i] == nullptr);
                branches_[i] = new StarTree(maxLeafSize_, splitPoint_,
                                            this);
                splitBounds(treeBounds_, branches_[i]->treeBounds_,
                            splitPoint_, static_cast<TreeDirection>(i));
            }

            // Add all the old stars
            for (vector<const Star*>::iterator it = stars_.begin();
                 it != stars_.end(); ++it) {
                dir = getDirection(splitPoint_, (*it)->position());
                branches_[dir]->addStar(*it);
            }
            // And don't forget the new one
            dir = getDirection(splitPoint_, star->position());
            branches_[dir]->addStar(star);
        } else {
            // Otherwise just add the star to the list
            stars_.push_back(star);
        }
    } else {
        // We're a branch, figure out the child that gets the star
        dir = getDirection(splitPoint_, star->position());
        branches_[dir]->addStar(star);
    }
}
} // namespace StarTree
