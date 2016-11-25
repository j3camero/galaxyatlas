#ifndef STARTREE_H_
#define STARTREE_H_

#include <vector>

#include <Eigen/Dense>

#include "star.h"

using std::vector;
using Eigen::Vector3d;

namespace startree {
// Make accessing Eigen vectors more clear
enum {
    X,
    Y,
    Z
};

/* Enumeration to represent the 8 divisions of an octree, the
 * "directions" one can take when traversing the tree */
typedef enum {
    NED,
    SED,
    NWD,
    SWD,
    NEU,
    SEU,
    NWU,
    SWU
} TreeDirection;

class StarTree {
public:
    StarTree(uint64_t maxLeafSize, Vector3d splitPoint,
             Vector3d minBounds, Vector3d maxBounds);

    // Add a star to the tree
    void addStar(const Star* star);

    // Getters
    bool isLeaf() const { return isLeaf_; }
    bool isRoot() const { return isRoot_; }
    const StarTree* parent() const { return parent_; }
    const StarTree* branch(TreeDirection dir) const
    { return branches_[dir]; }
    const vector<const Star*>& stars() const { return stars_; }

    uint64_t maxLeafSize() const { return maxLeafSize_; }
    uint64_t numStars() const { return numStars_; }
    double maxLuminosity() const { return maxLuminosity_; }
    double sumLuminosity() const { return sumLuminosity_; }
    const Vector3d& averageColor() const { return averageColor_; }

    const Vector3d& splitPoint() const { return splitPoint_; }
    const Vector3d& minTreeBounds() const { return treeBounds_[0]; }
    const Vector3d& maxTreeBounds() const { return treeBounds_[1]; }

    const Vector3d& minStarBounds() const { return starBounds_[0]; }
    const Vector3d& maxStarBounds() const { return starBounds_[1]; }
    const Vector3d& boundsCenter() const { return boundsCenter_; }
    double boundsRadius() const { return boundsRadius_; }
    
    // Get a TreeDirection from one point to another
    static TreeDirection getDirection(const Vector3d& from,
                                      const Vector3d& to);
private:
    // Used when splitting nodes, has no valid bounds
    StarTree(uint64_t maxLeafSize, StarTree* parent);
    
    // Adjust this tree's metadata to account for a new star
    void addStarMetadata(const Star* star);
    
    /* -- Children -- */
    bool isLeaf_; // Are we a leaf node?
    
    // If we're a leaf node, we have a list of Star object pointers
    vector<const Star*> stars_;

    // If we're a branch node, we have a split point
    Vector3d splitPoint_;
    
    /* If we're a branch node, we have an array of eight StarTree
     * object pointers */
    StarTree* branches_[8];

    /* -- Parent -- */
    bool isRoot_; // Are we the root node?
    StarTree* parent_;
    
    /* -- METADATA -- */
    // The maximum number of stars that may be contained in a leaf node
    const uint64_t maxLeafSize_;
    // Number of stars in this tree    
    uint64_t numStars_;
    // Luminosity of the brightest star in this tree
    double maxLuminosity_;
    // Sum of the luminocities of the stars in this tree
    double sumLuminosity_;

    // Sum of the RGB colors of the stars
    Vector3d sumColor_;
    // Average of the RGB colors of the stars
    Vector3d averageColor_;
    
    /* Bounds of the area technically covered by the tree,
     * there may not actually be stars at any of the boundaries.
     * First vector is min x,y,z. Second vector is max x, y, z. */
    Vector3d treeBounds_[2];
    
    /* Bounds of the area in which there are actually stars,
     * so there will be at least one star with one or more 
     * of the max/min coords.
     * First vector is min x,y,z. Second vector is max x, y, z. */
    Vector3d starBounds_[2];
    // Center of the starBounds
    Vector3d boundsCenter_;
    // Rough radius of the starBounds
    double boundsRadius_;
};

// Get all stars within a given radius from a point
void starsInRadius(const Vector3d& point, double radius,
                   vector<const StarTree*>& searchList,
                   vector<const Star*>& starsFound);

// Get all stars that would be individually visible from the given point
void visibleStars(const Vector3d& point, double minLuminosity,
                  vector<const StarTree*>& searchList,
                  vector<const Star*>& starsFound);

// Get all stars that match the magic formula
void visibleStarsMagic(const Vector3d& point, double minLuminosity,
                       double blurRadius,
                       vector<const StarTree*>& searchList,
                       vector<const Star*>& starsFound);

} // namespace StarTree

#endif // STARTREE_H_
