module Nova.Play where   -- no explicit exports because of shell.

--- this is a wrapper for Engine with dev commands used in the Shell.

import           Prelude hiding   (init)
-- the only code to use GLUT aside from Config.hs, Editor.hs
import qualified Graphics.UI.GLUT as GLUT
import           Graphics.UI.GLUT (($=),get)

import           System.Exit      (exitWith,ExitCode(ExitSuccess))
import           Control.Monad    (when)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef

import           Common           (justM)
import           Nova.GL
import           Nova.VectorOps   (Scalar)
import           Nova.Resource
import           Nova.Engine
import           Nova.Game

import           Nova.Draw
import qualified Nova.Draw as Draw

import           GrenadierBleu.GrenadierBleu


plEngine :: IORef (Engine GrenadierBleu)
plEngine = (_engine :: IORef (Engine GrenadierBleu))

{-
init :: Bool -> IO ()
init ss = do
    when ss $ Engine.loadStateNamed "autosave.dev" devEngine

    run

    GLUT.displayCallback       $= display
    GLUT.keyboardMouseCallback $= Just (Engine.keyboardMouse devEngine)
    GLUT.passiveMotionCallback $= Just (Engine.anyMotion     devEngine)
    GLUT.motionCallback        $= Just (Engine.anyMotion     devEngine)
  --GLUT.joystickCallback      $= Just (enginJoystick devEngine, 0)

shutdown :: Bool -> IO ()
shutdown b =
    when b $ do
                Engine.saveStateNamed "autosave.dev" devEngine
                -- TODO/FIXME: it doesn't work! GLUT is awful.
                get GLUT.currentWindow >>= justM GLUT.destroyWindow
                {-exitWith ExitSuccess-}
-}

{-
render g input = List [
    Enable PointSmooth,
    MatrixDo $ gRender (Engine.game en) (Engine.input en),
    if (not $ Engine.isRunning en) then List [Color4 1 1 1 0.25, FillBoxAt (1024, 1024) (0,0)] else NoDraw
    ]
-}

--------------------
-- shell commands
--------------------

run :: IO ()
run = GLUT.cursor $= GLUT.None >> waitForEvents plEngine False

pause :: IO ()
pause = GLUT.cursor $= GLUT.Inherit >> waitForEvents plEngine True

runUntil :: GLfloat -> IO ()
runUntil time = undefined

runFor :: Double -> IO ()
runFor seconds = undefined

{-
printGame :: IO ()
printGame = getEngine >>= \en-> (putStrLn $ show $ Engine.game en)

withGame f = getEngine >>= \en-> f $ Engine.game en
-}

speed :: Scalar -> IO ()
speed s = modifyEngine plEngine (setSpeed s)
