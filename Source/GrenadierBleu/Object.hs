module GrenadierBleu.Object where

import Nova.VectorOps
import Nova.Draw

class Object go where
    draw :: go -> Drawing
    move :: Scalar -> go -> go
    thinkSpawn :: Object gs => go -> [gs]

