module Nova.Winding where

import Data.List  (unzip3)
import Data.Maybe (catMaybes)

import Common     (rotatel)
import Nova.VectorOps
import Nova.HumaMath

data Step    = Step V2 V2 V4      -- texcoord, vertex, color
 deriving (Show,Read,Eq)

type Winding = [Step]

instance VectorOps Step where
    vecOf s   = Step (s,s) (s,s) (s,s,s,s)
    dot       = undefined
  --w `rot` a = rotAroundPointBy (centerVertex w) w a
    rot       = undefined

    linearLerp    (Step at av ac) (Step bt bv bc)                                 t =
        Step (linearLerp at bt       t) (linearLerp av bv       t) (linearLerp ac bc       t)

    quadraticLerp (Step at av ac) (Step bt bv bc) (Step ct cv cc)                 t =
        Step (quadraticLerp at bt ct t) (quadraticLerp av bv cv t) (quadraticLerp ac bc cc t)

    cubicLerp     (Step at av ac) (Step bt bv bc) (Step ct cv cc) (Step dt dv dc) t =
        Step (cubicLerp at bt ct dt  t) (cubicLerp av bv cv dv  t) (cubicLerp ac bc cc dc  t)

{-
    w `curvePlotSteps` s =
        let ts = (map sTexCoord w)  `curvePlotSteps` s in
        let vs = (map sVertex w) `curvePlotSteps` s in
        let cs = (map sColor w) `curvePlotSteps` s in
        map (\(t,v,c) -> Step t v c) $ zip3 ts vs cs
-}

instance Num Step where
    (Step at av ac) * (Step bt bv bc) = Step (at * bt ) (av * bv ) (ac * bc )
    (Step at av ac) + (Step bt bv bc) = Step (at + bt ) (av + bv ) (ac + bc )
    (Step at av ac) - (Step bt bv bc) = Step (at - bt ) (av - bv ) (ac - bc )
    negate (Step t v c)               = Step (negate t) (negate v) (negate c)

instance Fractional Step where
    (Step at av ac) / (Step bt bv bc) = Step (at / bt ) (av / bv ) (ac / bc )

modColor :: (V4 -> V4) -> Step -> Step
modColor f (Step t v c)  = Step t v (f c)

modVertex :: (V2 -> V2) -> Step -> Step
modVertex f (Step t v c) = Step t (f v) c

modParam :: (V2 -> V2) -> Step -> Step
modParam f (Step t v c)  = Step (f t) v c

asEdges :: Winding -> [(Step,Step)]
asEdges = spiral2

asLines :: Winding -> [Line]
asLines w = let vs = asVertices w in vs `zip` (rotatel vs)  -- just like spiral2
--asLines (asVertices -> vs) = vs `zip` (rotatel vs)

asVertices :: Winding -> [V2]
asVertices = map (\(Step _ v _) -> v)

spiral2 :: Winding -> [(Step,Step)]
spiral2 w = w `zip` (rotatel w)

spiral3 :: Winding -> [(Step,Step,Step)]
spiral3 ss = let ts = rotatel ss in zip3 ss ts (rotatel ts)
--spiral3 ss@(rotatel -> ts) = zip3 ss ts (rotatel ts)

mapEdges :: (Step -> Step -> a) -> Winding -> [a]
mapEdges f w = f `zipWith` w $ rotatel w

--mapLines :: (Line -> a) -> Winding -> [a]
--mapLines f w = let vs = asVertices w in zipWith f vs (transpose vs)

isConvex :: Winding -> Bool
isConvex w = (center w,0) `isCircleInside` w

isConcave :: Winding -> Bool
isConcave = not . isConvex

isCircleInside :: Circle -> Winding -> Bool
isCircleInside c = and . map (isCircleInFrontOfPlane c) . asLines

isCircleInSteps :: Circle -> Winding -> Bool
isCircleInSteps c = any (\v-> v `isPointInCircle` c) . asVertices

isCircleInEdges :: Circle -> Winding -> Bool
isCircleInEdges c = any (isCircleInLine c) . asLines

isWindingInside :: Scalar -> Winding -> Winding -> Bool
isWindingInside error a b =
    any (\v-> (v,error) `isCircleInside` b) (asVertices a)
 || any (\v-> (v,error) `isCircleInside` a) (asVertices b)

--indexesWith :: Winding -> (Step -> Bool) -> [(Winding,[Int])]
--indexesWith ps f = map (\w-> (w,findIndices f $ winding p)) ps

center :: Winding -> V2
center = average . asVertices

extentsOf :: Winding -> (V2,V2)
extentsOf w = ((left,bottom),(right,top)) where
    left    = foldr1 min xs
    right   = foldr1 max xs
    bottom  = foldr1 min ys
    top     = foldr1 max ys
    (xs,ys) = (unzip.asVertices) w

bounds :: Winding -> V2
--boundsOf w = let ((l,b),(r,t)) = extentsOf w in (r-l, t-b)
bounds = (\((l,b),(r,t)) -> (r-l, t-b)) . extentsOf

radius :: Winding -> Scalar
--radius = average bounds
radius w = let (w',h') = bounds w in (max w' h') / 2
--radius (bounds -> (w',h')) = (max w' h') / 2

firstCorner :: Winding -> V2
firstCorner = (\(Step _ v _) -> v) . head

secondCorner :: Winding -> V2
secondCorner = (\(Step _ v _) -> v) . (head . tail)

cutWithLine :: Winding -> Line -> (Maybe Winding,Maybe Winding)
-- should only be done on convex windings.
-- ...it isn't complete, it doesn't work as it should.
cutWithLine w cut = (splitW left,splitW right) where
    splitW f = let ss = split f in if null$ss then Nothing else Just ss
    left d   = d>=0
    right d  = d<=0
    --right (al,adist) (bl,bdist) = [] -- adist>0 && bdist<0
    cutSteps = concatMap
      (\(a@(Step _ av _),b@(Step _ bv _)) -> if (av,bv) `isLineInLine` cut then [a,average [a,b]] else [a]) $ asEdges w
    split f  = filter (\s@(Step _ v _) -> let (_,d) = v `distanceToPointOfLine` cut in f d) $ cutSteps

--subDivided :: 

