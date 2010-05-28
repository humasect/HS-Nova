module Nova.Game (
    Input(..),noInput,
    setInputChar,isCharDown,

    Game,Gaming(..),
    GameInfo(..),

    get,put,gets,modify
) where

import qualified Data.Map as Map
import Control.Monad.State.Lazy
import qualified Graphics.UI.GLUT as GLUT

import Nova.VectorOps (Scalar,V2,V3,V4,normalOf)
import Nova.Draw      (Drawing)

data Input = Input {
    upArrow, downArrow, leftArrow, rightArrow :: Bool,
    charStates :: Map.Map Char Bool,
    moveVector :: V2,

    leftMouseButton, middleMouseButton, rightMouseButton :: Bool,
    cursor, lastCursor, delta :: V2,
    screenCursor, lastScreenCursor, screenDelta :: V2
    }
    deriving (Show,Read)

noInput = Input {
    upArrow = False,downArrow=False,leftArrow=False,rightArrow=False,
    charStates = Map.empty,
    moveVector = (0,0),

    leftMouseButton=False,middleMouseButton=False,rightMouseButton=False,
    cursor = (0,0), lastCursor = (0,0), delta = (0,0),
    screenCursor = (0,0), lastScreenCursor = (0,0), screenDelta = (0,0)
    }

setInputChar :: Input -> Char -> Bool -> Input
setInputChar i x b = i { charStates = Map.insert x b (charStates i) }

isCharDown :: Input -> Char -> Bool
isCharDown i x = Map.findWithDefault False x (charStates i)

updateMovement :: Input -> Input
updateMovement i = i { moveVector = normalOf (move (leftArrow i) (rightArrow i), move (downArrow i) (upArrow i)) }
    where
    move False False =  0
    move True  _     = -1
    move _     True  =  1

-- Gaming g => 
type Game g a = StateT g IO a

class Gaming a where  -- Interactor a where
    newGame  :: a
    getInfo  :: a -> GameInfo       -- read only
    setInput :: a -> Input -> a     -- write only

--    gInit    :: BootMode -> Game a ()
    gUpdate  :: a -> Scalar -> a
    gRender  :: Game a Drawing
    gListen  :: a -> Game a ()

    gInputUp      :: GLUT.Key -> GLUT.Modifiers -> Game a ()
    gInputDown    :: GLUT.Key -> GLUT.Modifiers -> Game a ()
    gMouseDragged :: Game a ()
    gMouseMoved   :: Game a ()

    -- defaults

    gInputUp   _ _ = return ()
    gInputDown _ _ = return ()
    gMouseDragged  = return ()
    gMouseMoved    = return ()

data GameInfo = GameInfo {
    identifier :: String,
    gameWidth :: Scalar,
    gameHeight :: Scalar,
    gameBackground :: Maybe V4,
    camera :: V3

    -- add events here ..
    }
  deriving (Show,Read)
