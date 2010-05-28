module Nova.AudioStream (
    Stream,
    newStream,releaseStream,processStream
) where

import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as V
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable (peek)
import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad (when)
import Data.Int
import Data.IORef

import Common
import Nova.VectorOps (Scalar)
import Nova.HumaMath
import Nova.AL
import Nova.GL (FromConst(..),ToConst(..),glGen)

data Stream = Stream {
    source :: ALuint,
  --buffers :: (ALuint,ALuint)
    buffers :: [ALuint]
    }
    deriving (Show)

-- http://sugarpot.sakura.ne.jp/yuno/?OpenALストリーミング再生

-- code below translated and modded from http://www.devmaster.net/articles/openal-tutorials/lesson8.php

-- http://kcat.strangesoft.net/openal-tutorial.html

checkErrors :: String -> IO ()
checkErrors s = do
    e <- getError
    when (e /= (toConst NoError)) $ error $ "OpenAL error in '"++s++"': "++show e

open :: Int -> IO Stream
open bufs = do
    checkErrors "before open"

    src <- glGen genSources
    sourcef src (toConst Pitch) 1
    sourcef src (toConst Gain) 1
    source3f src (toConst Position) 0 0 0
    source3f src (toConst Velocity) 0 0 0
    source3f src (toConst Direction) 0 0 0
    sourcei src (toConst Looping) 0
    sourcef src (toConst RolloffFactor) 0
    sourcei src (toConst SourceRelative) 1

  --sourcei src (toConst Buffer) 0

    bs <- forM [0..(bufs-1)] $ \_-> glGen genBuffers

    checkErrors "open"

    return Stream { source=src, buffers=bs }

releaseStream :: Stream -> IO ()
releaseStream (Stream {source=src,buffers=bs}) = do
    sourceStop src
    with src $ deleteSources 1
    forM_ bs $ \b -> with b (deleteBuffers 1)

isPlaying :: Stream -> IO Bool
isPlaying (Stream {source=src}) = do
    st <- alloca (\ptr-> getSourcei src (toConst SourceState) ptr >> peek ptr)
  --putStrLn $ "playing: "++show st ++ " "++show (toConst Playing)
    return $ st == (toConst Playing)

_render :: ALuint -> IO ()
_render buf = do
--let v = V.replicate 1024 0 :: V.Vector Word16
    let s = cycle [0..90] :: [Scalar]
    let v = V.packWith (\a-> round $ ((sin (s !! a)) * 1000)) [0..2047] :: V.Vector Int16
    let (f,a,b) = V.toForeignPtr v

--putStrLn $ "a,b = "++show a ++ " " ++ show b ++ " buf=" ++ show buf
    withForeignPtr f (\ptr -> bufferData buf (toConst FormatMono16) ptr (fromIntegral b) 48000)
    checkErrors "render"

newStream :: Int -> IO Stream
newStream bufs = do
    s@(Stream {source=src,buffers=bs}) <- open bufs
    mapM_ (\b-> _render b) bs
  --with bs $ sourceQueueBuffers src (fromIntegral $ length bs)
    mapM_ (\b -> with b $ sourceQueueBuffers src 1) bs
    sourcePlay src
    checkErrors "newStream"
    return s

processStream :: Stream -> IO ()
processStream s@(Stream {source=src}) = do
    p <- alloca (\ptr-> getSourcei src (toConst BuffersProcessed) ptr >> peek ptr)
    if p<=0 then return () else do
        forM_ (replicate (fromIntegral p) 0) $ \_ -> do
            buf <- alloca (\ptr-> sourceUnqueueBuffers src 1 ptr >> peek ptr)
            _render buf
            with buf $ sourceQueueBuffers src 1
            checkErrors "fill queue"
        --b <- isPlaying s
        --if not b then sourcePlay src else return ()
        unlessM (isPlaying s) $ sourcePlay src
