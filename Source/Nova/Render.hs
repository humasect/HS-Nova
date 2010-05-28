module Nova.Render (
    renderPart,
    renderModel, --renderImage,
    renderProcedure
) where

import Control.Monad (foldM_)

import Common
import Nova.VectorOps
import Nova.HumaMath
import Nova.Winding
import Nova.Resource
import Nova.Procedural
import Nova.Synthesizer
import Nova.Mutator
import Nova.Part

import Nova.GL
import qualified Nova.GL as GL
import Nova.Draw (composeDrawing)

{-
renderContents :: Part -> Contents -> IO Part
renderContents p (Visual cs) = case c of
    Empty -> return p
    Proc pr -> renderProcedure pr p >> return p
    Synth s -> renderSynthesizer s p >> return p
  --Image i -> renderImage i (firstCorner $ processedWinding p) >> return p
    Textured i -> withResource i (\r -> draw $ TexturedDo (rTexID r) (procedure Default $ p {winding=processedWinding p})) >> return p
    Model m -> renderModel m (firstCorner $ processedWinding p) 0 >> return p
    Sound s -> return p -- findResource resMap s >>= \r-> renderSound r >> return p
    Trans m -> return p {winding=mutate m $ winding p}
-}

{-
renderContents :: Part -> Performer Procedural -> IO Part
renderContents p@(Part {winding=w}) (Trans m) = return p {winding=m `mutate` w}
renderContents p@(Part {winding=w}) (Renderer pr) = renderProcedure pr p >> return p
-}

renderPart :: Part -> IO ()
--renderPart p@(Part {perf=Visual cs}) = foldM_ renderContents p cs
renderPart _ = return ()

renderModel :: String -> V2 -> Scalar -> IO ()
renderModel s v a = withResource s $ \r-> do
    pushMatrix
    translate2v v
    rotate      a 0 0 1
    forM_ (rParts r) renderPart
    popMatrix

{-
renderImage :: String -> V2 -> IO ()
renderImage i v = withResource i $ \(RImage w h t) ->
    let (w',h') = (fromIntegral w, fromIntegral h) in
    texturedDo t (draw $ FillBoxAt (w'/2,h'/2) v)
-}

{-
renderResource :: String -> V2 -> IO ()
renderResource s v = findResource resMap s >>= \r-> case r of
    Just j@(RImage {}) -> color4 1 1 1 0.25 >> renderImageRepeated 4 j v
    Just (RModel {rParts=ps}) -> SmallerDo v 0.25 $ List [
            -- this is pretty much the same code in Render.hs , please fix and clean up after yourself.
            Translate (x,y)
            --forM_ ps renderPart
            ,undefined
            ]
    Nothing -> NoDraw
-}

renderProcedure :: Procedural -> Part -> IO ()
renderProcedure proc p = do
    --let b@(bw,bh) = bounds w
    --let (cx,cy) = center w

    --let w1 = (AddXY (-cx,-cy)) `mutate` prew
    --let w2 = (trans p) `mutate` w1
    --let w = mutate (Add (-cx,-cy)) prew

    --Draw.checkRenderErrors "before procedural"

    --pushMatrix
    --translate cx cy (fromIntegral $ layer p)

    when (isDepthProc proc) $ do
        enable    & DepthTest
        depthFunc & Always

        depthMask & True
        colorMask 0 0 0 0

        composeDrawing (Default `procedure` p)
        --forM_ w (draw.drawStep)

        depthMask & False
        colorMask 1 1 1 1

        depthFunc & GEqual

    composeDrawing $ proc `procedure` p

    when (isDepthProc proc) $ do
        depthMask & True
        clear     & DepthBufferBit
        depthMask & False
        disable   & DepthTest

    --popMatrix

    --Draw.checkRenderErrors $ "after procedural"

renderSynthesizer :: Synthesizer -> Part -> IO ()
renderSynthesizer s p = do
    return ()
{-
    let (cx,cy) = center prew
    let w = mutate (Add (-cx,-cy)) prew
    let x = s `synthesizer` (p {winding=w})
    return ()
-}
