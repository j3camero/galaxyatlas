#include <cmath>
#include <vector>
#include <iostream>
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
            const Vector3d splitPoint, Vector3d& childSplit,
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
    childSplit = 0.5 * (newBounds[0] + newBounds[1]);
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

StarTree::StarTree(uint64_t maxLeafSize, StarTree* parent) :
    isLeaf_(true),
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
    //std::cerr << "Start" << std::endl;
    numStars_ += 1;

    maxLuminosity_ = fmax(maxLuminosity_, star->lum());
    //std::cerr << "Set max luminocity." << std::endl;
    sumLuminosity_ += star->lum();
    //std::cerr << "Updated sum of luminocities." << std::endl;

    sumColor_ += star->color();
    //std::cerr << "Updated sum of colours." << std::endl;
    averageColor_ = sumColor_ / numStars_;
    //std::cerr << "Updated average colour." << std::endl;

    minVec(starBounds_[0], star->position());
    //std::cerr << "Updated star min bounds." << std::endl;
    maxVec(starBounds_[1], star->position());
    //std::cerr << "Updated star max bounds." << std::endl;

    boundsCenter_ = 0.5 * (starBounds_[0] + starBounds_[1]);
    //std::cerr << "Updated star bounds center." << std::endl;
    boundsRadius_ = (boundsCenter_ - starBounds_[1]).norm();
    //std::cerr << "Updated star bounds radius." << std::endl;
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
                branches_[i] = new StarTree(maxLeafSize_, this);
                splitBounds(treeBounds_, branches_[i]->treeBounds_,
                            splitPoint_, branches_[i]->splitPoint_,
                            static_cast<TreeDirection>(i));
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

static double getDistance(const Vector3d& pointA,
                          const Vector3d& pointB) {
    return (pointA - pointB).norm();
}

// Check if an octant approximately intersects the given sphere
static bool octantApproxIntersect(const Vector3d& point, double radius,
                                  const StarTree* t) {
    const double centerDistance = getDistance(point, t->splitPoint());
    const double subRadius = getDistance(t->splitPoint(),
                                         t->maxTreeBounds());
    return (radius + subRadius) >= centerDistance;
}

// Get all stars within a given radius from a point
void starsInRadius(const Vector3d& point, double radius,
                   vector<const StarTree*>& searchList,
                   vector<const Star*>& starsFound) {
    // Loop until the search list is empty
    while (searchList.size() > 0) {
        // Look at the first thing on the list
        const StarTree* t = searchList[0];
        searchList.erase(searchList.begin());

        // Does this octant possibly intersect our sphere?
        if (octantApproxIntersect(point, radius, t)) {
            if (t->isLeaf()) {
                // If it's a leaf check all the stars
                for (unsigned int i = 0; i < t->numStars(); i++) {
                    if (getDistance(point, (t->stars()[i])->position()) <=
                        radius) {
                        starsFound.push_back(t->stars()[i]);
                    }
                }
            } else {
                // If it's a branch, add all of the subnodes to be
                // searched
                for (int i = 0; i < 8; i++) {
                    const StarTree* tmp =
                        t->branch(static_cast<TreeDirection>(i));
                    searchList.push_back(tmp);
                }
            }
        }
    }
}

static bool canSeeStar(const Vector3d& point, double minLum,
                       const Star* star) {
    const Vector3d& starPosition = star->position();
    const double starDistance = getDistance(point, starPosition);
    const double starLuminosity = star->lum();

    return (starLuminosity /
            (4 * M_PI * starDistance * starDistance)) >= minLum;
}

static bool hasVisibleStars(const Vector3d& point, double minLum,
                            const StarTree* t) {
    const double centerDistance = getDistance(point, t->splitPoint());
    const double subRadius = getDistance(t->splitPoint(),
                                         t->maxTreeBounds());
    const double minDistance = fmax((centerDistance - subRadius), 0);

    return (t->maxLuminosity() /
            (4 * M_PI * minDistance * minDistance)) >= minLum;
}

void visibleStars(const Vector3d& point, double minLuminosity,
                  vector<const StarTree*>& searchList,
                  vector<const Star*>& starsFound) {
    // Loop until the search list is empty
    while (searchList.size() > 0) {
        // Look at the first thing on the list
        const StarTree* t = searchList[0];
        searchList.erase(searchList.begin());

        if (hasVisibleStars(point, minLuminosity, t)) {
            if (t->isLeaf()) {
                for (unsigned int i = 0; i < t->numStars(); i++) {
                    if (canSeeStar(point, minLuminosity, t->stars()[i])) {
                        starsFound.push_back(t->stars()[i]);
                    }
                }
            } else {
                for (int i = 0; i < 8; i++) {
                    const StarTree* tmp =
                        t->branch(static_cast<TreeDirection>(i));
                    searchList.push_back(tmp);
                }
            }
        }
    }
}

static double sigmoid(double x) {
    return 1.0 / (1.0 + exp(x));
}

static double magicFormula(double distance, double blurRadius) {
    double a = log(distance) - log(blurRadius);
    double b = sigmoid((-2.03935397) * a);
    double c = sigmoid(-3.065390091 * a + 1.851112427);

    return ( (b + 1.872968808 * c * (1.0 - c)) /
             (4 * M_PI * distance * distance) );
}

static bool canSeeStarMagic(const Vector3d& point, double minLum,
                            double blurRadius, const Star* star) {
    const Vector3d& starPosition = star->position();
    const double starDistance = getDistance(point, starPosition);
    const double starLuminosity = star->lum();
    const double magic =
        magicFormula(starDistance, blurRadius) * starLuminosity;
    
    return magic >= minLum;
}

static bool hasVisibleStarsMagic(const Vector3d& point, double minLum,
                                 double blurRadius, const StarTree* t) {
    const double centerDistance = getDistance(point, t->splitPoint());
    const double subRadius = getDistance(t->splitPoint(),
                                         t->maxTreeBounds());
    const double minDistance = fmax((centerDistance - subRadius),
                                    (4.84814e-7));
    const double magic =
        magicFormula(minDistance, blurRadius) * t->maxLuminosity();

    return magic >= minLum;
}

void visibleStarsMagic(const Vector3d& point, double minLuminosity,
                       double blurRadius,
                       vector<const StarTree*>& searchList,
                       vector<const Star*>& starsFound) {
    // Loop until the searchlist is empty
    while (searchList.size() > 0) {
        // Look at the first thing on the list
        const StarTree* t = searchList[0];
        searchList.erase(searchList.begin());
        
        if (hasVisibleStarsMagic(point, minLuminosity, blurRadius, t)) {
            if (t->isLeaf()) {
                for (unsigned int i = 0; i < t->numStars(); i++) {
                    if (canSeeStarMagic(point, minLuminosity, blurRadius,
                                        t->stars()[i])) {
                        starsFound.push_back(t->stars()[i]);
                    }
                }
            } else {
                for (int i = 0; i < 8; i++) {
                    const StarTree* tmp =
                        t->branch(static_cast<TreeDirection>(i));
                    searchList.push_back(tmp);
                }
            }
        }
    }
}
} // namespace StarTree
