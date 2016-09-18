 -- Specialized Octree for StarData

module StarTree where

import Data.List

import Data.Vector.V3
import Data.Vector.Class
import Debug.Trace

import qualified StarData as SD
import Color

-- Jeff's Magic Formula (TM) for selecting stars
magicFormula :: Double -> Double -> Double
magicFormula viewDist blurRad =
  let a     = log(viewDist) - log(blurRad)
      b     = sigmoid ((-2.03935397) * a)
      c     = sigmoid (-3.065390091 * a + 1.851112427)
  in
   (b + 1.872968808 * c * (1.0 - c)) /
   (viewDist * viewDist)

-- Helper for magic formula
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + (exp x))

-- Maximum number of stars that can be in a leaf node
maxStarsInLeaf = 8 :: Integer

-- Enumeration type for octant directions
data ODir = NED | SED | NWD | SWD | NEU | SEU | NWU | SWU
          deriving (Eq, Ord, Enum, Show, Bounded)

-- Give an enumeration direction from ca to cb
cmp :: Vector3 -> Vector3 -> ODir
cmp ca cb = joinStep (cx, cy, cz)
  where cx = v3x ca >= v3x cb
        cy = v3y ca >= v3y cb
        cz = v3z ca >= v3z cb

-- Helper for cmp
joinStep :: (Enum a1, Enum a3, Enum a2, Enum a) => (a1, a2, a3) -> a
joinStep (cx, cy, cz) =
  toEnum ( (fromEnum cx) +
           (2 * fromEnum cy) +
           (4 * fromEnum cz) )

-- Rectangular prism boundaries defined by max and min (x,y,z)
data RectBounds = RectBounds
                  { minCoords :: Vector3
                  , maxCoords :: Vector3
                  }
                deriving (Show,Eq)

-- Empty boundary record, not a valid bounding box
emptyBounds :: RectBounds
emptyBounds = RectBounds (Vector3 0 0 0) (Vector3 0 0 0)

-- Metadata for a tree branch or leaf
data TreeMetaData = TreeMetaData
                    { treeBounds    :: RectBounds
                    , numStars      :: Integer
                    , maxLum        :: Double
                    , starBounds    :: RectBounds
                    , sumLum        :: Double
                    , sumRGB        :: Color
                    , avgRGB        :: Color
                    }
                  deriving Show

-- StarTree leaf node
data LeafNode a = LeafNode
                  { leafMetaData :: TreeMetaData
                  , starList     :: [a]
                  }
                deriving Show

-- StarTree branch node
data OctreeNode a = OctreeNode
                    { splitPoint :: Vector3
                    , branchMetaData :: TreeMetaData
                    , ned :: StarTree a -- (+x, +y, +z)
                    , sed :: StarTree a -- (-x, +y, +z)
                    , swd :: StarTree a -- (-x, -y, +z)
                    , nwd :: StarTree a -- (+x, -y, +z)
                    , neu :: StarTree a -- (+z, +y, -z)
                    , seu :: StarTree a -- (-x, +y, -z)
                    , swu :: StarTree a -- (-x, -y, -z)
                    , nwu :: StarTree a -- (+x, -y, -z)
                    }
                deriving Show

-- Container type for the leaves and branches
-- I could have used Either, and I may yet.
data StarTree a = Leaf (LeafNode a) | Branch (OctreeNode a)
                deriving Show


-- Get subtree accessor function from direction
subTreeGetterFromODir :: (SD.StarData a) =>
                         ODir -> (OctreeNode a -> StarTree a)
subTreeGetterFromODir NED = ned
subTreeGetterFromODir NWD = nwd
subTreeGetterFromODir SED = sed
subTreeGetterFromODir SWD = swd
subTreeGetterFromODir NEU = neu
subTreeGetterFromODir NWU = nwu
subTreeGetterFromODir SEU = seu
subTreeGetterFromODir SWU = swu

-- Get subtree setter function for tree from direction
subTreeSetterFromODir :: (SD.StarData a) =>
                         OctreeNode a -> ODir ->
                         (StarTree a -> OctreeNode a)
subTreeSetterFromODir node@(OctreeNode {}) dir =
  case dir of
   NED -> \sub -> node { ned = sub }
   NWD -> \sub -> node { nwd = sub }
   SED -> \sub -> node { sed = sub }
   SWD -> \sub -> node { swd = sub }
   NEU -> \sub -> node { neu = sub }
   NWU -> \sub -> node { nwu = sub }
   SEU -> \sub -> node { seu = sub }
   SWU -> \sub -> node { swu = sub }

-- Get subtree from an octree node and an octant direction
subTreeFromODir :: (SD.StarData a) =>
                   OctreeNode a -> ODir -> StarTree a
subTreeFromODir node dir =
  (subTreeGetterFromODir dir) node

-- Add a new star to the metadata of a tree node
addStarMetaData :: (SD.StarData a) =>
                   TreeMetaData -> a -> TreeMetaData
addStarMetaData meta star =
  let
    newNumStars = (numStars meta) + 1
    newSumBrite = (sumLum meta) + (SD.starLum star)
    newSumRGB   = addColors (sumRGB meta) (SD.starRGB star)
    newAvgRGB   = scaleColor newSumRGB (1 / (fromIntegral newNumStars))
    minAList = zip (vunpack . minCoords . starBounds $ meta)
               [(SD.starX star), (SD.starY star), (SD.starZ star)]
    maxAList = zip (vunpack . maxCoords . starBounds $ meta)
               [(SD.starX star), (SD.starY star), (SD.starZ star)]
    -- We know these can't fail
    Just newMinBounds =
      (vpack $ map (\(a,b) -> min a b) $ minAList) :: Maybe Vector3
    Just newMaxBounds =
      (vpack $ map (\(a,b) -> max a b) $ maxAList) :: Maybe Vector3
    nMaxLum  = max (SD.starLum star) (maxLum meta)
  in
   meta { numStars = newNumStars
        , maxLum = nMaxLum
        , starBounds = (RectBounds newMinBounds newMaxBounds)
        , sumLum = newSumBrite
        , sumRGB = newSumRGB
        , avgRGB = newAvgRGB
        }

-- Calculate new bounding box given old bounds, split point and octant
newBoundsFromODir :: RectBounds -> Vector3 -> ODir -> RectBounds
newBoundsFromODir (RectBounds minVec maxVec) spltPnt octant =
  case octant of
   NED -> (RectBounds spltPnt maxVec)
   NWD -> (RectBounds (spltPnt {v3y = (v3y minVec)})
           (maxVec {v3y = (v3y spltPnt)}))
   SED -> (RectBounds (spltPnt {v3x = (v3x minVec)})
           (maxVec {v3x = (v3x spltPnt)}))
   SWD -> (RectBounds (minVec {v3z = (v3z spltPnt)})
           (spltPnt {v3z = (v3z maxVec)}))
   NEU -> (RectBounds (spltPnt {v3z = (v3z minVec)})
           (maxVec {v3z = (v3z spltPnt)}))
   NWU -> (RectBounds (minVec {v3x = (v3x spltPnt)})
           (spltPnt {v3x = (v3x maxVec)}))
   SEU -> (RectBounds (minVec {v3y = (v3y spltPnt)})
           (spltPnt {v3y = (v3y maxVec)}))
   SWU -> (RectBounds minVec spltPnt)

-- Create a new empty leaf
emptyLeaf :: (SD.StarData a) => RectBounds -> StarTree a
emptyLeaf bounds =
  Leaf $ LeafNode (TreeMetaData bounds 0 0 emptyBounds 0
                   (RGB 0 0 0)
                   (RGB 0 0 0)) []

-- Absolute outer boundaries (hard coded for HYG dataset)
-- TODO: Fixme
outerBounds =
  let minx = (-100000.0)
      miny = (-100000.0)
      minz = (-100000.0)
      maxx = ( 100000.0)
      maxy = ( 100000.0)
      maxz = ( 100000.0)
  in
   RectBounds (Vector3 minx miny minz) (Vector3 maxx maxy maxz)

-- Create a new empty star tree (meant to be the root)
emptyStarTree :: (SD.StarData a) => StarTree a
emptyStarTree =
  let splt = (Vector3 0 0 0)
  in
   Branch $
   OctreeNode
   splt
   (TreeMetaData outerBounds 0 0 emptyBounds 0 (RGB 0 0 0) (RGB 0 0 0))
   (emptyLeaf $ newBoundsFromODir outerBounds splt NED)
   (emptyLeaf $ newBoundsFromODir outerBounds splt SED)
   (emptyLeaf $ newBoundsFromODir outerBounds splt SWD)
   (emptyLeaf $ newBoundsFromODir outerBounds splt NWD)
   (emptyLeaf $ newBoundsFromODir outerBounds splt NEU)
   (emptyLeaf $ newBoundsFromODir outerBounds splt SEU)
   (emptyLeaf $ newBoundsFromODir outerBounds splt SWU)
   (emptyLeaf $ newBoundsFromODir outerBounds splt NWU)

-- Put a star into a leaf node
addStarToLeaf :: (SD.StarData a) => LeafNode a -> a -> LeafNode a
addStarToLeaf leaf@(LeafNode meta stars) star =
  let newMeta = addStarMetaData meta star
  in
   leaf { leafMetaData = newMeta,
          starList = (stars ++ [star]) }

-- Generate an OcreeNode from a LeafNode by splitting it
treeNodeFromLeaf :: (SD.StarData a) => LeafNode a -> StarTree a
treeNodeFromLeaf leaf@(LeafNode meta stars) =
  let -- Figoure out the split point
    minVec = (minCoords . treeBounds) $ meta
    maxVec = (maxCoords . treeBounds) $ meta
    splt   = (maxVec + minVec) / (fromIntegral 2)
    -- Create an empty OctreeNode
    bounds = treeBounds meta -- Same bounds as leaf had
    emeta = TreeMetaData bounds 0 0 emptyBounds 0
            (RGB 0 0 0) (RGB 0 0 0)-- an empty metadata
    node  = OctreeNode
            splt emeta
            (emptyLeaf $ newBoundsFromODir bounds splt NED)
            (emptyLeaf $ newBoundsFromODir bounds splt SED)
            (emptyLeaf $ newBoundsFromODir bounds splt SWD)
            (emptyLeaf $ newBoundsFromODir bounds splt NWD)
            (emptyLeaf $ newBoundsFromODir bounds splt NEU)
            (emptyLeaf $ newBoundsFromODir bounds splt SEU)
            (emptyLeaf $ newBoundsFromODir bounds splt SWU)
            (emptyLeaf $ newBoundsFromODir bounds splt NWU)
  in
   -- Add all the stars to the new branch node
   foldl' addStar (Branch node) stars

-- Add a star to a StarTree
addStar :: (SD.StarData a) => StarTree a -> a -> StarTree a
addStar (Leaf (leaf@(LeafNode {}))) star =
  if (numStars . leafMetaData $ leaf) < maxStarsInLeaf then
    Leaf (addStarToLeaf leaf star)
  else
    addStar (treeNodeFromLeaf leaf) star
addStar (Branch (ocnode@(OctreeNode {}))) star =
  let dir    = cmp (splitPoint ocnode) (SD.positionVec star)
      newsub = addStar ((subTreeGetterFromODir dir) ocnode) star
      nmeta  = addStarMetaData (branchMetaData ocnode) star
  in
   Branch ((subTreeSetterFromODir (ocnode {
                                      branchMetaData = nmeta
                                      }) dir) $ newsub)

-- Add a list of stars to a StarTree
addStarList :: (SD.StarData a) => StarTree a -> [a] -> StarTree a
addStarList tree [] = tree
addStarList tree (star:stars) =
  addStarList (addStar tree star) stars

-- Turn a list of stars into a StarTree
fromList :: (SD.StarData a) => [a] -> StarTree a
fromList starData =
  addStarList emptyStarTree starData

-- Distance between Vector points
distV3 :: Vector3 -> Vector3 -> Double
distV3 a b = vmag (a - b)

-- Is there any chance these tree sections possibly intersect?
octantApproxIntersect :: (SD.StarData a) =>
                         Vector3 -> Double -> StarTree a -> Bool
octantApproxIntersect pnt rng (Branch oct@(OctreeNode {})) =
  let cntrDst = distV3 pnt (splitPoint oct)
      octRad  = distV3 (splitPoint oct)
                (maxCoords . treeBounds . branchMetaData $ oct)
  in
   (rng + octRad) >= cntrDst
octantApproxIntersect pnt rng (Leaf lf@(LeafNode meta stars)) =
  let bounds  = treeBounds meta
      cntr    = ((maxCoords bounds) + (minCoords bounds)) /
                (fromIntegral 2)
      cntrDst = distV3 pnt cntr
      lfRad   = distV3 cntr (maxCoords bounds)
  in
   (rng + lfRad) >= cntrDst


-- Tail recursive inRadius search function
inRadiusTC :: (SD.StarData a) =>
              Vector3 -> Double -> [StarTree a] -> [a] -> [a]
-- If nothing left to check, return
inRadiusTC pnt rng [] acc = acc
-- Recursive case
inRadiusTC pnt rng (t:ts) acc =
  case t of
   brch@(Branch oct@(OctreeNode {})) ->
     if (octantApproxIntersect pnt rng brch) then
       inRadiusTC pnt rng (ts ++ [(ned oct), (nwd oct),
                                  (sed oct), (swd oct),
                                  (neu oct), (nwu oct),
                                  (seu oct), (swu oct)]) acc
     else
       inRadiusTC pnt rng ts acc
   lf@(Leaf (LeafNode meta stars)) ->
     if (octantApproxIntersect pnt rng lf) then
       let inRad star = let sPos = SD.positionVec star
                            dist = distV3 pnt sPos
                        in dist <= rng
           newAcc = acc ++ (filter inRad stars)
       in
        inRadiusTC pnt rng ts newAcc
     else
       inRadiusTC pnt rng ts acc

-- Filter a StarTree for stars within range of a point
inRadius :: (SD.StarData a) => Vector3 -> Double -> StarTree a -> [a]
inRadius pnt rng tree =
  inRadiusTC pnt rng [tree] []

{-
inRadius pnt rng brch@(Branch oct@(OctreeNode {})) =
  if (octantApproxIntersect pnt rng brch) then
    -- Filter the subtrees
    (inRadius pnt rng (ned oct)) ++
    (inRadius pnt rng (nwd oct)) ++
    (inRadius pnt rng (sed oct)) ++
    (inRadius pnt rng (swd oct)) ++
    (inRadius pnt rng (neu oct)) ++
    (inRadius pnt rng (nwu oct)) ++
    (inRadius pnt rng (seu oct)) ++
    (inRadius pnt rng (swu oct))
  else
    []
inRadius pnt rng lf@(Leaf (LeafNode meta stars)) =
  if (octantApproxIntersect pnt rng lf) then
    -- Filter the leaf
    filter (\star -> let sPos = SD.positionVec star
                         dist = distV3 pnt sPos
                     in dist <= rng) stars
  else
    []
-}

-- Returns true if the branch or leaf contains at least 1
-- star above the threshold apparent luminocity
hasVisibleStars :: (SD.StarData a) =>
                   Vector3 -> Double -> StarTree a -> Bool
hasVisibleStars pnt lum (Branch oct@(OctreeNode {})) =
  let cntrDst = distV3 pnt (splitPoint oct)
      octRad  = distV3 (splitPoint oct)
                (maxCoords . treeBounds . branchMetaData $ oct)
      minDst  = max (cntrDst - octRad) 0
      mLum    = maxLum . branchMetaData $ oct
  in
   (mLum / (minDst * minDst)) >= lum
hasVisibleStars pnt lum (Leaf lf@(LeafNode meta stars)) =
  let bounds  = treeBounds meta
      cntr    = ((maxCoords bounds) + (minCoords bounds)) /
                (fromIntegral 2)
      cntrDst = distV3 pnt cntr
      lfRad   = distV3 cntr (maxCoords bounds)
      minDst  = max (cntrDst - lfRad) 0
      mLum    = maxLum meta
  in
   (mLum / (minDst * minDst)) >= lum

-- Tail recursive visibleStars search function
visibleStarsTC :: (SD.StarData a) =>
                  Vector3 -> Double -> [StarTree a] -> [a] -> [a]
-- If nothing left to check, return
visibleStarsTC pnt lum [] acc = acc
-- Recursive case
visibleStarsTC pnt lum (t:ts) acc =
  case t of
   brch@(Branch oct@(OctreeNode {})) ->
     if (hasVisibleStars pnt lum brch) then
       visibleStarsTC pnt lum (ts ++ [(ned oct), (nwd oct),
                                      (sed oct), (swd oct),
                                      (neu oct), (nwu oct),
                                      (seu oct), (swu oct)]) acc
     else
       visibleStarsTC pnt lum ts acc
   lf@(Leaf (LeafNode meta stars)) ->
     if (hasVisibleStars pnt lum lf) then
       let canSee star = let sPos = SD.positionVec star
                             dist = distV3 pnt sPos
                             slum = SD.starLum star
                         in (slum / (dist * dist)) >= lum
           newAcc = acc ++ (filter canSee stars)
       in
        visibleStarsTC pnt lum ts newAcc
     else
       visibleStarsTC pnt lum ts acc

-- Given a minimum apparent luminocity, return all
-- stars visible from the given point
visibleStars :: (SD.StarData a) =>
                StarTree a -> Vector3 -> Double -> [a]
visibleStars tree pnt lum =
  visibleStarsTC pnt lum [tree] []

{-
visibleStars brch@(Branch oct@(OctreeNode {})) pnt lum =
  if (hasVisibleStars pnt lum brch) then
    -- Filter the subtrees
    (visibleStars (ned oct) pnt lum) ++
    (visibleStars (nwd oct) pnt lum) ++
    (visibleStars (sed oct) pnt lum) ++
    (visibleStars (swd oct) pnt lum) ++
    (visibleStars (neu oct) pnt lum) ++
    (visibleStars (nwu oct) pnt lum) ++
    (visibleStars (seu oct) pnt lum) ++
    (visibleStars (swu oct) pnt lum)
  else
    []
visibleStars lf@(Leaf (LeafNode meta stars)) pnt lum =
  if (hasVisibleStars pnt lum lf) then
    -- Filter the leaf
    filter (\star -> let sPos = SD.positionVec star
                         dist = distV3 pnt sPos
                         slum = SD.lum star
                     in (slum / (dist * dist)) >= lum) stars
  else
    []
-}

-- Returns true if the branch or leaf contains at least 1
-- star above the threshold apparent luminocity
-- (Uses magic formula as brightness multiplier)
hasVisibleStarsMagic :: (SD.StarData a) =>
                        Vector3 -> Double -> Double -> StarTree a ->
                        Bool
hasVisibleStarsMagic pnt lum brad (Branch oct@(OctreeNode {})) =
  let cntrDst = distV3 pnt (splitPoint oct)
      octRad  = distV3 (splitPoint oct)
                (maxCoords . treeBounds . branchMetaData $ oct)
      minDst  = max (cntrDst - octRad) (4.84814e-7)
      mLum    = (maxLum . branchMetaData $ oct)
      magic   = (magicFormula minDst brad) * mLum
  in
   magic >= lum
hasVisibleStarsMagic pnt lum brad (Leaf lf@(LeafNode meta stars)) =
  let bounds  = treeBounds meta
      cntr    = ((maxCoords bounds) + (minCoords bounds)) /
                (fromIntegral 2)
      cntrDst = distV3 pnt cntr
      lfRad   = distV3 cntr (maxCoords bounds)
      minDst  = max (cntrDst - lfRad) (4.84814e-7)
      mLum    = maxLum meta
      magic   = (magicFormula minDst brad) * mLum
  in
   magic >= lum

-- Tail recursive visibleStars search function
visibleStarsMagicTC :: (SD.StarData a) => Vector3 -> Double -> Double ->
                       [StarTree a] -> [a] -> [a]
-- If nothing left to check, return
visibleStarsMagicTC pnt lum brad [] acc = acc
-- Recursive case
visibleStarsMagicTC pnt lum brad (t:ts) acc =
  case t of
   brch@(Branch oct@(OctreeNode {})) ->
     if (hasVisibleStarsMagic pnt lum brad brch) then
       visibleStarsMagicTC pnt lum brad (ts ++ [(ned oct), (nwd oct),
                                                (sed oct), (swd oct),
                                                (neu oct), (nwu oct),
                                                (seu oct), (swu oct)]) acc
     else
       visibleStarsMagicTC pnt lum brad ts acc
   lf@(Leaf (LeafNode meta stars)) ->
     if (hasVisibleStarsMagic pnt lum brad lf) then
       let canSee star = let sPos = SD.positionVec star
                             dist = distV3 pnt sPos
                             slum = SD.starLum star
                             mgic = (magicFormula dist brad) * slum
                         in mgic >= lum
           newAcc = acc ++ (filter canSee stars)
       in
        visibleStarsMagicTC pnt lum brad ts newAcc
     else
       visibleStarsMagicTC pnt lum brad ts acc

-- Given a minimum apparent luminocity, return all
-- stars visible from the given point
visibleStarsMagic :: (SD.StarData a) =>
                     StarTree a -> Vector3 -> Double -> Double -> [a]
visibleStarsMagic tree pnt lum brad =
  visibleStarsMagicTC pnt lum brad [tree] []

{-
visibleStarsMagic brch@(Branch oct@(OctreeNode {})) pnt lum brad =
  if (hasVisibleStarsMagic pnt lum brad brch) then
    -- Filter the subtrees
    (visibleStarsMagic (ned oct) pnt lum brad) ++
    (visibleStarsMagic (nwd oct) pnt lum brad) ++
    (visibleStarsMagic (sed oct) pnt lum brad) ++
    (visibleStarsMagic (swd oct) pnt lum brad) ++
    (visibleStarsMagic (neu oct) pnt lum brad) ++
    (visibleStarsMagic (nwu oct) pnt lum brad) ++
    (visibleStarsMagic (seu oct) pnt lum brad) ++
    (visibleStarsMagic (swu oct) pnt lum brad)
  else
    []
visibleStarsMagic lf@(Leaf (LeafNode meta stars)) pnt lum brad =
  if (hasVisibleStarsMagic pnt lum brad lf) then
    -- Filter the leaf
    filter (\star -> let sPos = SD.positionVec star
                         dist = distV3 pnt sPos
                         slum = SD.lum star
                         mgic = (magicFormula dist brad) * slum
                     in mgic >= lum) stars
  else
    []
-}

-- Test function
cmptest :: Bool
cmptest = let v0 = Vector3 0 0 0
              vNED = Vector3 ( 1) ( 1) ( 1)
              vNWD = Vector3 ( 1) (-1) ( 1)
              vSED = Vector3 (-1) ( 1) ( 1)
              vSWD = Vector3 (-1) (-1) ( 1)
              vNEU = Vector3 ( 1) ( 1) (-1)
              vNWU = Vector3 ( 1) (-1) (-1)
              vSEU = Vector3 (-1) ( 1) (-1)
              vSWU = Vector3 (-1) (-1) (-1)
          in
           ( ((cmp v0 vNED) == NED) &&
             ((cmp v0 vNWD) == NWD) &&
             ((cmp v0 vSED) == SED) &&
             ((cmp v0 vSWD) == SWD) &&
             ((cmp v0 vNEU) == NEU) &&
             ((cmp v0 vNWU) == NWU) &&
             ((cmp v0 vSEU) == SEU) &&
             ((cmp v0 vSWU) == SWU)
           )
