module Nova.Part (
    Performance(..),Part(..),
    emptyPart,isPartInside,

    lerpTravel,
    processedWinding,
    Procedural(..),PerfStep(..)
) where

import Nova.HumaMath
import Nova.VectorOps
import {-# SOURCE #-} Nova.Synthesizer
import Nova.Mutator
import Nova.Winding

{-
data Contents = Empty
    | Proc Procedural | Synth Synthesizer | Trans Mutator
    | Textured String | Model String | Image String | Sound String
    deriving (Show,Read,Eq)
-}

data Procedural =
      Default
    | Gradient CompassDirection
    | Textured String | Model String | Image String

    -- for editor
    | EditNote String | ResPreview (Maybe String) | ResMaking (Maybe String)
    | Wired | WireColored | Bounds | TravelKnot | StepVecs Scalar
  deriving (Show,Read,Eq)

data PerfStep a = Trans Mutator | Render a
  deriving (Show,Read,Eq)

type Process a = [PerfStep a]

data Performance =
      Visual   (Process  Procedural)
    | Ethereal (Process     Mutator)
    | Aural    (Process Synthesizer)
--    | Material (Process    Resource)
  deriving (Show,Read,Eq)

{-
data Performance =
      Visual  String (Process Procedural)
    | VisualD String (Process Procedural)
-}

type Travel = V3 -- loc, speed (revs per sec), end (1=full circle, 10=ten times)

unitTravel :: Travel
unitTravel = (0,1,1)

lerpTravel :: Part -> Step
lerpTravel (Part {winding=w,travel=(loc,rps,end)}) =
    let n = length w                  in
    let e = asEdges w !! truncate loc in
  --Step (0,0) (0,0) (1,0,0,1)
    head w

data Part = Part {
    layer   :: Int,
    flags   :: String,
    perf    :: Performance,
    travel  :: Travel,
    winding :: Winding,
    trans   :: [Mutator]
    }
  deriving (Show,Read,Eq)

instance Ord Part where a `compare` b = (layer a) `compare` (layer b)

emptyPart :: Part
emptyPart = Part {
    layer   = 0,
    flags   = "",
    perf    = Ethereal [],
    travel  = unitTravel,
    winding = [],
    trans   = []
  }

isPartInside :: Scalar -> Part -> Part -> Bool
isPartInside error (Part {winding=a}) (Part {winding=b}) = isWindingInside error a b

processedWinding :: Part -> Winding
processedWinding (Part {winding=w,trans=ms}) = (Many ms) `mutate` w

{-
flaggedPartsForModel :: String -> String -> IO [Part]
flaggedPartsForModel name flag = do
    liftM (\r-> case r of
        RModel {rParts=ps} -> filter (\(Part {flags=f}) -> f == flag) ps
        _ -> error ("Procedural.windingsForProc: no model '" ++ name ++ "'.") >> [])
        $ findResource resMap name
-}

