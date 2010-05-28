module Nova.Render where

import Nova.Resource
import Nova.VectorOps
import Nova.Part

renderPart :: Part -> IO ()
renderModel :: String -> V2 -> Scalar -> IO ()
--renderImage :: String -> V2 -> IO ()

renderProcedure :: Procedural -> Part -> IO ()
