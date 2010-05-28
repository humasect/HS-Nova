module Nova.Dynamics (
) where

import Nova.HumaMath
import Nova.Winding

data World = World {
     wBodies :: [Body],
    wGravity :: V2
    }
  deriving (Show,Read)

data Body = Body {
    bOrigin :: V2,
       bVel :: V2,

      bMass :: Scalar,
      bHull :: Hull
    }
  deriving (Show,Read)

-- box, line, ellipse, image ....
data Hull = WindingHull Winding | CircleHull Circle | PointHull V2 | LineHull Line
  deriving (Show,Read)

isCollide :: Hull -> Hull -> Bool
isCollide (WindingHull a) (WindingHull b) = isWindingInside 0 a b
isCollide (CircleHull  a) (CircleHull  b) = isCircleInCircle  a b
isCollide (PointHull   a) (PointHull   b) = a == b
isCollide (LineHull    a) (LineHull    b) = a `isLineInLine` b

isCollide   (WindingHull w)   (CircleHull  s) = s `isCircleInside` w
isCollide s@(CircleHull  _) w@(WindingHull _) = isCollide w s
isCollide   (WindingHull w)   (PointHull   p) = (p,0) `isCircleInside` w
isCollide p@(PointHull   _) w@(WindingHull _) = isCollide w p
isCollide   (CircleHull  s)   (PointHull   p) = p `isPointInCircle` s
isCollide p@(PointHull   _) s@(CircleHull  _) = isCollide s p

isCollideList :: Hull -> [Hull] -> Bool
isCollideList h hs = any (isCollide h) hs

collideAgainst :: Body -> Body -> Body
collideAgainst a b = if (bHull a) `isCollide` (bHull b) then a else a

step :: Scalar -> Body -> Body
step tick b@(Body {bOrigin=o,bVel=v}) = b {bOrigin= o + v * vecOf tick}

--simulate :: [RigidBody] -> [RigidBody]
--simulate rbs = map (\rb -> collideAgainst ) rbs

--simulate :: World -> World
--simulate w@(World {wBodies=bs,wGravity=g}) = w { wBodies = 
  --map (\b -> if 
