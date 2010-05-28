module Nova.Orbedit where

--import Graphics.UI.GLUT.Menu
--import Graphics.UI.GLUT (($=),Modifiers(..),KeyState(..),Key(..),SpecialKey(..),MouseButton(..))
import Graphics.UI.GLUT (($=))
import qualified Graphics.UI.GLUT as GLUT

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Nova.GL
import Nova.HumaMath
import Nova.Resource
import Nova.Render

import Nova.EditCommon
import Nova.PVUniverse

data Orbedit = Orbedit {
	universe :: CelestialBody,
	camera :: V3,
	cursor :: V2,
	lastCursor :: V2
	}

orbedit :: IORef Orbedit
orbedit = unsafePerformIO $ newIORef Orbedit {
	universe = testUniverse,
	camera = (0,0,10),
	cursor = (0,0),
	lastCursor = (0,0)
	}

getOrbedit = readIORef orbedit
modifyOrbedit = modifyIORef orbedit
putOrbedit = writeIORef orbedit

display :: IO ()
display = do
	--startFrame (-512,511) (-320,319)
	startFrame 1024 640 (0,0,0)
	or <- getOrbedit

	withCamera (camera or) $ do
		renderGrid 1
		drawCelestialBody (universe or) (0,0)

	GLUT.swapBuffers

input key GLUT.Down modifiers (GLUT.Position px py) = do
	GLUT.postRedisplay Nothing
	or <- getOrbedit

	putOrbedit =<< case key of
		GLUT.SpecialKey GLUT.KeyLeft -> move or (-8, 0, 0)
		GLUT.SpecialKey GLUT.KeyRight -> move or (8, 0, 0)
		GLUT.SpecialKey GLUT.KeyUp -> move or (0, 8, 0)
		GLUT.SpecialKey GLUT.KeyDown -> move or (0, -8, 0)
		_ -> return or

	where
	move or v = return or {camera=(camera or) <+> v}

input key GLUT.Up modifiers (GLUT.Position px py) = return ()

motion (GLUT.Position px py) = return ()

passiveMotion (GLUT.Position px py) = do
	or <- getOrbedit
	let p = translateScreenPos (camera or) 1 (px,py)
	putOrbedit or {cursor=p, lastCursor=p}

open :: String -> IO ()
open name = return ()

init :: Bool -> IO ()
init ss = do
	GLUT.keyboardMouseCallback $= Just input
	GLUT.displayCallback $= display
	GLUT.motionCallback $= Just motion
	GLUT.passiveMotionCallback $= Just passiveMotion

shutdown :: Bool -> IO ()
shutdown ss = return ()
