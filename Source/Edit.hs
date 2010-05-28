module Nova.Edit (
    engine,bootInit,bootShutdown
) where

import Prelude hiding     (init)
import Graphics.UI.GLUT   (($=),Modifiers(..),KeyState(..),Key(..),SpecialKey(..),MouseButton(..))
import qualified Graphics.UI.GLUT as GLUT
import Data.Maybe
import Data.IORef
import System.Directory   (getDirectoryContents,doesFileExist)
import System.Exit
import Control.Monad.State.Lazy

import Common
import Config             (translateScreenPos)
import Nova.HumaMath
import Nova.Winding
import Nova.Resource
import qualified Nova.Mutator as MU
import qualified Nova.Engine as Engine
import Nova.Part
import Nova.Engine
import Nova.Game
import Nova.Draw

import Editor.Editor
import Editor.Operations (reloadFile)
import Editor.Display
import Editor.Events

instance Gaming Editor where
    newGame           = newEditor
    getInfo        ed = gameInfo ed
    setInput     ed i = ed { input = i }

    gRender           = render
    gUpdate   ed tick = ed { seconds = seconds ed + tick }
    gListen   _       = return ()

    gInputUp      = inputUp
    gInputDown    = inputDown
    gMouseDragged = mouseDragged
    gMouseMoved   = mouseMoved

instance BootEngine Editor where
    engine = (_engine :: IORef (Engine Editor))

    bootInit (Edit model) en = do
        --waitForEvents en True
        case model of
            Nothing   -> return en
            Just this -> execStateT reloadFile (game en) {name=this} >>= \ed -> return en {game=ed}

    bootShutdown en = return ()

{-
setupCursor :: GLUT.Position -> Game Editor ()
setupCursor (GLUT.Position px py) =
    get >>= \ed@(Editor {cursor=(c,curErr)}) ->

    let sp = translateScreenPos (px,py)          in
    let cp = (camera ed) `translateCameraPos` sp in
    let  p = cp `snapToGrid` (gridSize ed)       in

    put ed {cursor=(p,curErr), lastCursor=c, screenCursor=sp, lastScreenCursor=(screenCursor ed)}
-}

{-
runInputEvent :: Editor () -> GLUT.Position -> IO ()
runInputEvent st p =
    getEditor >>= execStateT (setupCursor p >> st >> setupCursor p) >>= \ed->
    when (needsDisplay ed) (GLUT.postRedisplay Nothing) >>
    putEditor ed {needsDisplay = False}

keyboardMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
keyboardMouse key Up   modifiers p = runInputEvent (inputUp   key modifiers) p
keyboardMouse key Down modifiers p = runInputEvent (inputDown key modifiers) p
-}

{-
			MenuEntry "Select Touching" $ return (),
			MenuEntry "Invert Selection" $ edit invertSelection

			MenuEntry "Select Touching " $edit$ selectAllFrom (\s w-> False),
			MenuEntry "Select Matching Content" $edit$ selectAllFrom (\s w-> (contents w) == (contents s)),
			MenuEntry "Select Step Count" $edit$ selectAllFrom (\s w-> (length$steps w) == (length$steps s)),
			MenuEntry "Select Same Layer" $edit$ selectAllFrom (\s w-> layer w == layer s),
			MenuEntry "Invert Selection" $edit$ invertSelection

		MenuEntry "Set Color 0%" $ setColors 0,
		MenuEntry "Set Color 50%" $ setColors 0.5,
		MenuEntry "Set Color 100%" $ setColors 1,

		MenuEntry "Snap All to Grid" snapAll,
		MenuEntry ("Export Selection to '"++expModel++"'") $ exportSel artDir writeModel (expName++".model"),
		MenuEntry ("Export Selection to '"++expNVM++"'") $ exportSel contentDir writeNVM (expName++".nvm")

			MenuEntry "Select All" $edit$ selectAll (\w -> True),
			MenuEntry "Select All Visible" $edit$ selectVisible,
			MenuEntry "Select All Non-curves" $edit$ selectAll (\w-> not$isCurved w),
			MenuEntry "Select All Curves" $edit$ selectAll (\w-> isCurved w) ]),

			MenuEntry "480x640 3:4" $ edit (\ed->ed{border=Just (480,640)}),
			MenuEntry "640x480 4:3" $ edit (\ed->ed{border=Just (640,480)}),
			MenuEntry "640x640 1:1" $ edit (\ed->ed{border=Just (640,640)}),
			MenuEntry "No Border" $ edit (\ed->ed{border=Nothing})]),
-}