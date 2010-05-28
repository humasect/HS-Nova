module Nova.Procedural (
    Procedural(..),
    procedure,isDepthProc
) where

import Common
import Nova.HumaMath
import Nova.VectorOps (vecOf)
import Nova.Winding
import Nova.Part
import Nova.Resource
import Nova.Winding

import           Nova.Draw
import qualified Nova.GL as GL

procedure :: Procedural -> Part -> Drawing
procedure  Default     p = DrawWinding GL.TriangleFan (winding p)

--------- editor procedures (unprocessed winding)

procedure (EditNote s) p = List [Color3 (1,1,1), TextAt (firstCorner$winding p) s]

procedure Wired p@(Part {trans=ms,winding=w}) = let vs = asVertices w in List [
    --Begin GL.LineLoop, List $ map Vertex2v vs, End,
    draw $ map (uncurry Arrow) (asLines w),
    Begin GL.Points, draw vs, End,

    case ms of
        [] -> NoDraw
        _  -> List [Color3 (1,0,1), Begin GL.LineLoop, draw (asVertices $ processedWinding p), End]
    ]

procedure WireColored p@(Part {trans=ms,winding=w}) = List [
    --DrawWinding GL.LineLoop w,
    draw $ map (uncurry Arrow) (asLines w),
    DrawWinding GL.Points w,
  --if b then List [Color3 1 0 1, PR.drawWinding GL.LineLoop (w `curvePlotSteps` 16)] else NoDraw
    case ms of
        [] -> NoDraw
        _  -> List [Color3 (1,0,1), DrawWinding GL.LineLoop (processedWinding p)]
    ]

procedure Bounds p@(Part {winding=w}) =
    let (bw,bh) = bounds w in
    let c@(x,y) = center w in
    List [
        LineBoxAt (bw/2,bh/2) c, -- (0,0) --c
        Begin GL.LineLoop, draw $ (firstCorner $ processedWinding p,16) `plotCircleSteps` (0,16), End,
        RenderProc (StepVecs 4) p
        ]


procedure TravelKnot p@(Part {travel=t,winding=w}) =
{-  pushMatrix
    let (cx,cy) = firstCorner w
    translate cx cy 0-}

    let s@(Step _ v _) = lerpTravel p in List [Begin GL.Points, Vertex2 v, End]

  --popMatrix

procedure (StepVecs s) p = List [
    Begin GL.Lines,
    List $ concatMap (\(Step t v _) -> [Vertex2 v, Vertex2 (v + t * vecOf s)]) $ winding p,
    End
    ]

{-
procedure (ResPreview   Nothing) _ = NoDraw
procedure (ResPreview (Just [])) _ = NoDraw
procedure (ResPreview (Just rp)) p = do
    let c = (average.asVertices.winding) p
    rcts <- findResource resMap rp
    case rcts of
                              Just j@(RImage {})        -> List [Color4 1 1 1 0.25, ImageRepeated 1 j c]
                              Just (RModel {rParts=ps}) -> SmallerDo c 0.25 $ List [
                            -- this is pretty much the same code in Render.hs , please fix and clean up after yourself.
                                                               Translate c,
                                                             --drawFor ps renderPart,
                                                               List $ map RenderPart ps]
                              Nothing                   -> NoDraw
                            --Just                    w -> render w


procedure (ResMaking   Nothing) _ = NoDraw
procedure (ResMaking (Just [])) _ = NoDraw
procedure (ResMaking (Just rp)) p = do
    let c = (average.asVertices.winding) p
    rcts <- findResource resMap rp
    case rcts of
      Just j@(RImage {}) -> List [Color4 1 1 1 0.33, ImageRepeated 6 j c]
      Nothing            -> NoDraw
-}

procedure _ _ = NoDraw

isDepthProc :: Procedural -> Bool
isDepthProc _ = False

