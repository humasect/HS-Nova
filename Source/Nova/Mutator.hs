module Nova.Mutator where

import Common
import Nova.HumaMath
import Nova.Winding
import Nova.VectorOps

data Mutator =
      Static

    -- these prefer (length w > 0)
    | Many [Mutator] | AtCenterDo Mutator
    | AutoCurve Int | AutoIncurve Int  --  | Subdivide Int
    | OpenFan | ClosedFan -- these specially use rendering mode TriangleFan instead of Polygon

    | Multiply V2       | Add V2
    | MultiplyStep Step | AddStep Step
    | CenterRotate Scalar     | RotateAround V2 Scalar

    | LightHead Scalar Scalar | LightTail Scalar Scalar

    | FlipX | FlipY | FlipU | FlipV | Invert | RotateStepsL | RotateStepsR

    -- creates geometry
    | Star Scalar | NinjaStar Scalar

    -- editor
    | AppendStep Step | SnapToGrid Scalar
  deriving (Show,Read,Eq)

data StepOp =
    Unit | Addx Step | Mulx Step -- ...
  deriving (Show,Read)

unmutated :: Mutator -> Mutator
unmutated  Static         = Static
unmutated (Many       ms) = Many $ reverse ms
unmutated (AtCenterDo ms) = undefined

mutate :: Mutator -> Winding -> Winding

--mutate (AppendStep s) w = w ++ [s]
mutate (AppendStep s) w = if (null w) || (last w /= s) then w++[s] else w

mutate  _ [] = []

mutate  Static        w = w
mutate (Many  (m:ms)) w = mutate m (mutate (Many ms) w)
mutate (Many      []) w = w
mutate (AtCenterDo m) w = mutate (Many [Add (center w),m,Add (negate$center w)]) w

mutate (AutoCurve   steps) w = w `curvePlotSteps` steps
mutate (AutoIncurve steps) w = reverse $ (mutate (AutoCurve steps) w) ++ (tail $ reverse w)
--mutate (Subdivide steps) w = undefined

mutate  OpenFan   w = average w : w
mutate  ClosedFan w = (mutate OpenFan w) ++ [head w]

mutate (Multiply     s) w = for w $ modVertex (*s)
mutate (Add          s) w = for w $ modVertex (+s) -- \(Step t v c) -> Step t (v+s) c
mutate (MultiplyStep s) w = for w (* s)
mutate (AddStep      s) w = for w (+ s)

mutate (CenterRotate   a) w = mutate (RotateAround (center w) a) w
mutate (RotateAround s a) w = for w $ modVertex (\v -> rotAround v a s)

mutate (LightHead 1 1) w = w
mutate (LightHead s a) w = modColor (* (s,s,s,a)) (head w) : tail w
mutate (LightTail 1 1) w = w
mutate (LightTail s a) w = head w : map (modColor (* (s,s,s,a))) (tail w)

mutate  FlipX        w = for w $ modVertex (\(x,y) -> (-x,y))
mutate  FlipY        w = for w $ modVertex (\(x,y) -> (x,-y))
mutate  FlipU        w = for w $ modParam  (\(u,v) -> (-u,v))
mutate  FlipV        w = for w $ modParam  (\(u,v) -> (u,-v))
mutate  Invert       w = (reverse.rotatel) w
mutate  RotateStepsL w = rotatel w
mutate  RotateStepsR w = rotater w

mutate (SnapToGrid s) w = for w $ modVertex (\v -> v `snapToGrid` s)

mutate (NinjaStar t) w = let c = average w in concatMap (\s-> [s,linearLerp s c t]) w
mutate (Star      t) w = let c = average w in concatMap (\(a,b)-> [a,linearLerp ((a+b)/vecOf 2) c t]) (spiral2 w)

curvePlotSteps :: VectorOps a => [a] -> Int -> [a]
vs `curvePlotSteps` steps = let p = plot steps in
        case vs of
            a:b:c:d:e:f:g:h:i:_ -> (p$cubicLerp a b c d) ++ (p$cubicLerp d e f g) ++ (p$cubicLerp g h i a)
            a:b:c:d:e:f:g:h:_   -> (p$cubicLerp a b c d) ++ (p$cubicLerp d e f g) ++ (p$quadraticLerp g h a)
            a:b:c:d:e:f:g:_     -> (p$cubicLerp a b c d) ++ (p$quadraticLerp d e f) ++ (p$quadraticLerp f g a)
            a:b:c:d:e:f:_       -> (p$cubicLerp a b c d) ++ (p$cubicLerp d e f a)
            a:b:c:d:e:_         -> (p$cubicLerp a b c d) ++ (p$quadraticLerp d e a)
            a:b:c:d:_           -> p$cubicLerp a b c d
            a:b:c:_             -> p $ quadraticLerp a b c
            _                   -> [] --circlePoints steps (centerOf w, radiusOf w)

{-
rotateContentsBy :: ResourceMap -> Scalar -> Winding -> Winding
rotateContentsBy rm a w = case (findResource rm $ contents w) of
    Nothing -> w
    Just (RImage {rWidth=w',rHeight=h'}) ->
        let (x,y) = centerOf w in
        let uv = (x/(fromIntegral w'),y/(fromIntegral h')) in
        w {steps=map (\(Step t v col) -> (Step (rotatedAround t a uv) v col)) (steps w)}
    Just (RModel {}) -> w
-}

