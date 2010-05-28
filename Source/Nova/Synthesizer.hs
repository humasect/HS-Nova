module Nova.Synthesizer (
    Synthesizer(..),
    synthesizer
) where

import Nova.VectorOps
import Nova.HumaMath
import Nova.Part

data Synthesizer = Silent | Wavetable String | Volume Scalar | SineOsc Scalar
    deriving (Show,Read,Eq)

synthesizer :: Synthesizer -> Part -> [Scalar]
synthesizer Silent _ = []
synthesizer (SineOsc freq) w = map sin (iterate (2*pi*freq +) 0)



-- http://en.wikipedia.org/wiki/Subtractive_synthesis

{-

There are some details to be aware of. First, sine function value ranges from -1 to 1, so it must be rescaled to cover the entire sample range that is, in 16 bits, -2^15 to 2^15. With 8 bits sound samples, this scale value would be different (i.e., -2^7 to 2^7). Another thing to note is how the time is computed. Since the sin(x) function takes radian value as input, the time variable must grow by exactly 2*pi in one second in order to generate a 1Hz sine wave. If the sin(x) function input value would have been in degrees instead of radians, then the time variable would grow by 360 (degrees) each second. The variable m_step is the time increment per sample.
-}

-- drums make a pitch sweep. starting at higher freq and lowering until 0 amp.

-- http://dafx04.na.infn.it/WebProc/Proc/P_201.pdf

{-
superpose :: Num a => [a] -> [a] -> [a]
superpose = zipWith (+)

exponential :: Num a => a -> [a]
exponential decay = iterate (decay*) 1

amplitude :: (Num a, Ord a) => [a] -> a
amplitude x = foldl max 0 (map abs x)

echo :: Num a => Int -> a -> [a] -> [a]
echo time gain x = let y = superpose x (delay time (amplify gain y)) in y

delay :: Num a => Int -> [a]
delay time = (replicate time 0 ++)

oscillator :: Scalar -> [Scalar]
oscillator freq = map sin (iterate (2*pi*freq +) 0)

bell :: Scalar -> Scalar -> [Scalar]
bell decay freq = zipWith (*) (exponential decay) (oscillator freq)
-}

--type Generator = Time -> [Scalar]
-- white noise
-- oscillator sine wave
-- oscillator square wave
-- oscillator triangle wave
-- wave table

--type Effect = Scalar -> Time -> Scalar
-- LFO
-- envelope / amplitude
-- mix (polyphony)
-- phaser (simple recursive filter that emphasizes a specific frequency)
-- filter envelope. (works on params. ?)


