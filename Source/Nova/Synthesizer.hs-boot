module Nova.Synthesizer where

import Nova.VectorOps

data Synthesizer = Silent | Wavetable String | Volume Scalar | SineOsc Scalar

instance Show Synthesizer
instance Read Synthesizer
instance Eq Synthesizer
