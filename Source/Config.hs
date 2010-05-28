{-# OPTIONS_GHC -cpp #-}
module Config (
    rootDir,artDir,contentDir,
    
    titleBarHeight,
    translateScreenPos,

    -- for Shell
    ghcPath
) where

import           System.FilePath
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Environment (getEnv)

import Nova.VectorOps (Scalar,V2)
--import Nova.GL        (GLint)
import Graphics.UI.GLUT (GLint)

-- these for main and buildcontent and shell
rootDir:: FilePath
rootDir    = let h = unsafePerformIO $ getEnv "HOME" in h++"/Novapilot/"

contentDir = joinPath [rootDir, "Content/"]
artDir     = joinPath [rootDir, "Art/"]

-- this for the shell
ghcPath:: FilePath
--ghcPath = "/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-6.8.3/"
ghcPath    = "/Users/humasect/Haskell/lib/ghc-6.10.0.20081005/"

{-
titleBarHeight :: Int
#ifdef DARWIN
titleBarHeight = 23
#endif
 -}

-- title bar heights! silly glut.
-- Apple
#define TBH 22
titleBarHeight :: GLint
titleBarHeight = TBH

--wWidth = unsafePerformIO $ newIORef 0

translateScreenPos         :: (GLint,GLint) -> V2
-- a,b,c = camera+scale
-- PROBLEM: glut makes window. includes titlebar in window height. does not include title bar in 
--          cursor position. therefore, squished.
--          window size: 640+titleBarHeight == 662
--          pos at top of window, just under titlebar = 0.
--          pos at bottom of window == 662.
--
-- .. dirtiest function in the entire codebase.
--
translateScreenPos (px,py) = (fromIntegral x', fromIntegral y')
    where
    ww       = 512      -- editor
  --ww       = 320      -- dev
    x'       = px - ww --512
    glutFixY:: GLint
    glutFixY = let fact = ((fromIntegral $ 640+titleBarHeight) / 640) :: Scalar in round $ (fromIntegral py) / fact
    y'       = (640 - glutFixY) - 320
