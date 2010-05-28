{-# OPTIONS_GHC -XTypeSynonymInstances #-}
module Nova.HumaMath where

import Data.List (zip4,unzip4)
import Debug.Trace

import Nova.VectorOps

-- |Translate using @(cx,cy,scale)@ camera a point @v@.
translateCameraPos :: V3 -> V2 -> V2
translateCameraPos (a,b,c) (cx,cy) = ((cx+(a*c))/c, (cy+(b*c))/c)

data CompassDirection = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    deriving (Eq,Show,Read,Enum)

succCompDir   :: CompassDirection -> CompassDirection
succCompDir cd = if cd == NorthWest then North else succ cd

predCompDir   :: CompassDirection -> CompassDirection
predCompDir cd = if cd == North then NorthWest else pred cd

vectorForCompDir  :: CompassDirection -> V2
vectorForCompDir d = normalOf $ case d of
    North     -> ( 0, 1)
    NorthEast -> ( 1, 1)
    East      -> ( 1, 0)
    SouthEast -> ( 1,-1)
    South     -> ( 0,-1)
    SouthWest -> (-1,-1)
    West      -> (-1, 0)
    NorthWest -> (-1, 1)

angleForCompDir :: CompassDirection -> Scalar
angleForCompDir d = case d of
    North     ->    0
    NorthEast ->  -45
    East      ->  -90
    SouthEast -> -135
    South     ->  180
    SouthWest ->  135
    West      ->   90
    NorthWest ->   45

angleForPoints                :: V2 -> V2 -> Scalar
angleForPoints (x0,y0) (x1,y1) = atan2 (y1-y0) (x1-x0)

toDegrees :: Scalar -> Scalar
toDegrees r = r * (180/pi)

keepPointInBox              :: V2 -> V2 -> V2
keepPointInBox (bx,by) (x,y) = (if x<0 then max x (-bx) else min x bx, if y<0 then max y (-by) else min y by)

isPointInBox              :: V2 -> V2 -> Bool
isPointInBox (x,y) (bx,by) = not (x<(-bx) || x>bx || y<(-by) || y>by)

isPointInSquare                      :: V2 -> (V2,V2) -> Bool
isPointInSquare (x,y) ((w,h),(bx,by)) = (x>=bx && x<=(bx+w)) && (y>=by && y<=(by+h))

isPointInCircle        :: V2 -> Circle -> Bool
isPointInCircle a (b,r) = (magnitudeOf (b - a)) <= r

isCircleInCircle              :: Circle -> Circle -> Bool
isCircleInCircle (a,ar) (b,br) = (magnitudeOf (b - a)) <= (ar+br)

isCircleInBox :: Circle -> V2 -> Bool
isCircleInBox (a,r) b = isPointInBox a (b + vecOf r)


{-

planeForPoints :: V3 -> V3 -> V3 -> V4
--planeForPoints a b c = undefined
    --let (x,y,z) = v3normal (v3scale (v3cross (v3sub c a) (v3sub c b)) (-1)) in (x,y,z, ((x,y,z) `v3dot` a))
planeForPoints a b c =
    let (x,y,z) = normalOf (((c <-> a) `v3cross` (c <-> b)) <*> (-1)) in (x,y,z, (x,y,z) <.> a)

inFrontOfPlane :: V3 -> V4 -> Bool
v `inFrontOfPlane` (x,y,z,w) = ((v <.> (x,y,z)) - w) >= -0.001
-}

type Line = (V2,V2)

distanceToPointOfLine :: V2 -> Line -> (V2,Scalar)
distanceToPointOfLine (cx,cy) ((ax,ay),(bx,by)) =
    let rnumer = (cx-ax)*(bx-ax) + (cy-ay)*(by-ay)          in
    let rdenom = (bx-ax)*(bx-ax) + (by-ay)*(by-ay)          in
    let r      = rnumer / rdenom                            in
    let p      = (ax + r*(bx-ax), ay + r*(by-ay))           in
    let s      = ((ay-cy)*(bx-ax)-(ax-cx)*(by-ay)) / rdenom in

    if r >= 0 && r <= 1 then (p,s * sqrt rdenom) else
        let d1 = (cx-ax)*(cx-ax) + (cy-ay)*(cy-ay) in
        let d2 = (cx-bx)*(cx-bx) + (cy-by)*(cy-by) in
        if d1 < d2 then ((ax,ay), sqrt d1) else ((bx,by), sqrt d2)

isLineInLine :: Line -> Line -> Bool
isLineInLine (av,bv) cut =
    let (_,adist) = av `distanceToPointOfLine` cut in
    let (_,bdist) = bv `distanceToPointOfLine` cut in
    (adist<=0 && bdist>=0) || (adist>=0 && bdist<=0)

isCircleInLine :: Circle -> Line -> Bool
isCircleInLine (c,error) line = let (_,dist) = distanceToPointOfLine c line in (abs dist) <= error

type Circle = (V2,Scalar)

distanceToPointOfPlane :: V2 -> Line -> Scalar
distanceToPointOfPlane (cx,cy) ((x0,y0),(x1,y1)) = (cy-y0)*(x1-x0) - (cx-x0)*(y1-y0)

isCircleInFrontOfPlane :: Circle -> Line -> Bool
isCircleInFrontOfPlane (c,error) l = (c `distanceToPointOfPlane` l) <= error

plotCircleSteps :: Circle -> (Int,Int) -> [V2]
(p,r) `plotCircleSteps` (skip,steps) =
                            let step = 360 / (fromIntegral steps) in
                            let arm = (normalOf (1,1)) * vecOf r  in
--    trace ("a b =  - "++show skip ++" "++show steps)
--    let pts = [ (v `rot` (-((*) step $ fromIntegral n))) + p | n <- [skip..steps] | v <- repeat arm ] in
--    trace (show pts) pts
    map (\(n,v) -> (v `rot` (-((*) step $ fromIntegral n))) + p) $ zip [skip..steps] (repeat arm)

--circle :: Circle -> Scalar -> Scalar
--circle ((x,y),r) t = 

type Ellipse = (V2,Scalar)

ellipse :: Ellipse -> Scalar -> V2
ellipse ((a,b),angle) time =
                     let beta = (-angle) / 180 * pi in
                  let step = 360 * time          in
                     let alpha = step / 180 * pi     in
    ((a * (cos alpha) * (cos beta) - b * (sin alpha) * (sin beta)),
     (a * (cos alpha) * (sin beta) + b * (sin alpha) * (cos beta)))

ellipsePoints :: Int -> Ellipse -> V2 -> [V2]
ellipsePoints steps e v = [ (ellipse e $ (fromIntegral n)/(fromIntegral steps)) + v | n <- [0..steps] ]

--isCircleInEllipse :: Circle -> Ellipse -> Bool
--isCircleInEllipse ((x,y),error) e = False

plot :: Int -> (Scalar -> a) -> [a]
plot steps f = [ f $ (fromIntegral n)/(fromIntegral steps) | n <- [0..steps] ]
