module Nova.Engine (
    Engine(..),_engine,
    modifyEngine,modifyGame,ioEngine,
    setSpeed,
    
    BootEngine(..),BootMode(..),

    install,uninstall,

    waitForEvents,usingStatesNamed
) where

import System.CPUTime
import Control.Monad      (when)
import Data.IORef
import Control.Concurrent (yield)
import System.IO.Unsafe   (unsafePerformIO)
import Control.Monad.State.Lazy

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))
import Sound.OpenAL.ALC 
import Sound.OpenAL.AL            hiding (stop)  -- ??

import Config             (translateScreenPos)
import Common
import Nova.VectorOps
import Nova.HumaMath
import Nova.Game
import Nova.Resource
import Nova.AudioStream
import Nova.Files
import Nova.Draw          (startFrame,endFrame,composeDrawing)

data BootMode = Play | Edit (Maybe String) | Orbedit (Maybe String)

class (Read g,Show g,Gaming g) => BootEngine g where
    engine :: IORef (Engine g)
    bootInit     :: BootMode -> Engine g -> IO (Engine g)
    bootShutdown :: Engine g -> IO ()

    engine = _engine

    bootInit  _ en = return en
    bootShutdown _ = return ()

data Gaming g => Engine g = Engine {
    lastCPUTime :: Int,
     inputState :: Input,
           game :: g,

    isInstalled :: Bool,
  waitingEvents :: Bool,
   savingStates :: Maybe String,

        tickMul :: Scalar,
         stream :: Maybe Stream
    }

newEngine :: Gaming g => Engine g
newEngine = Engine {
    lastCPUTime = 0,
     inputState = noInput,
           game = newGame,

    isInstalled = False,
  waitingEvents = False,
   savingStates = Nothing,

        tickMul = 1,
         stream = Nothing
    }

{-# NOINLINE _engine #-}
_engine :: Gaming g => IORef (Engine g)
_engine = unsafePerformIO $ newIORef newEngine

--Gaming g => 
--type EngineT g a = StateT (Engine g) IO a

modifyEngine :: Gaming g => IORef (Engine g) -> (Engine g -> Engine g) -> IO ()
modifyEngine _en f = liftM f (readIORef _en) >>= writeIORef _en

ioEngine :: Gaming g => IORef (Engine g) -> (Engine g -> IO (Engine g)) -> IO ()
ioEngine _en f = readIORef _en >>= f >>= writeIORef _en

--execGame :: Gaming g => IORef (Engine g) -> Game g () -> IO ()
--execGame _en f = ioEngine $ \en -> execStateT f (game en)

modifyGame :: Gaming g => IORef (Engine g) -> (g -> g) -> IO ()
modifyGame _en f = modifyEngine _en $ \en -> en {game=f$game en}

step :: Gaming g => Engine g -> IO (Engine g)
step en@(Engine {game=g,waitingEvents=wait}) = if not wait then step' en else return en

step' en = do
    now <- GLUT.get GLUT.elapsedTime

    let start = (fromIntegral $ lastCPUTime en)
    let   end = (fromIntegral now)
    let  tick = (end - start)/1000

  --color3 1 1 1
  --renderTextAt (100,100) ("fps: " ++ (show fps))

    s   <- case (stream en) of
             Nothing -> newStream 2
             Just j  -> processStream j >> return j
    
    let g = (game en) `setInput` (inputState en)
    g' <- execStateT (gListen g) (game en)

    let nextg = gUpdate g' (tick * tickMul en)
    return en {lastCPUTime=now, game=nextg, stream=Just s}

setSpeed :: Gaming g => Scalar -> Engine g -> Engine g
setSpeed s en = en {tickMul=if s>0 then s else tickMul en}

{-
keyboardMouse :: Game g => IORef (Engine g) -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
keyboardMouse _en key state _ _ = do
    modifyIORef _en updateInput

        Char 'p' -> do
            b <- gets isRunning
            liftIO $ waitForEvents edEngine b
            modify $ \ed -> ed {isRunning=not b}


    -- debug keys
    when (state == GLUT.Down) $ case key of
        GLUT.Char '\ESC' -> toggleState _en
        GLUT.Char 'p'    -> toggleState _en
      --GLUT.Char 'q'    -> exitWith ExitSuccess

        GLUT.Char ','    -> saveStateNamed "snapshot" _en
        GLUT.Char '.'    -> loadStateNamed "snapshot" _en
                          --loadFrom (game en) "snapshot.dev" >>= \g-> writeIORef _en en {game=g}
        GLUT.Char '-'    -> modifyIORef _en $ \en -> setSpeed en (tickMul en - 0.1)
        GLUT.Char '='    -> modifyIORef _en $ \en -> setSpeed en (tickMul en + 0.1)

        _                -> return ()

    where
    updateInput en@(Engine {isRunning=False}) = en
    updateInput en@(Engine {isRunning= True, input=i}) =
        let isDown = if state == GLUT.Down then True else False in
        let i' = case key of
                    GLUT.SpecialKey GLUT.KeyLeft       -> i {  left=isDown }
                    GLUT.SpecialKey GLUT.KeyRight      -> i { right=isDown }
                    GLUT.SpecialKey GLUT.KeyUp         -> i {    up=isDown }
                    GLUT.SpecialKey GLUT.KeyDown       -> i {  down=isDown }
                    GLUT.Char x                        -> inputChar i x isDown
                    GLUT.MouseButton GLUT.LeftButton   -> i {   leftButton=isDown }
                    GLUT.MouseButton GLUT.MiddleButton -> i { middleButton=isDown }
                    GLUT.MouseButton GLUT.RightButton  -> i {  rightButton=isDown }
                    _                                  -> i

        in en {input=updateMovement i'}

    -- Editor.hs has this too
    toggleState _en = readIORef _en >>= \en-> if isRunning en then uninstall _en else install _en

-}

usingStatesNamed :: (Gaming g,Show g) => IORef (Engine g) -> Maybe String -> IO ()
_en `usingStatesNamed` stateName = do
    saveState _en
    modifyIORef _en $ \en -> en {savingStates=stateName}

saveState :: (Gaming g,Show g) => IORef (Engine g) -> IO ()
saveState _en = do
    en@(Engine {game=g}) <- readIORef _en
    case savingStates en of
        Nothing   -> return ()
        Just this -> g `saveAs` (this ++ "." ++ (identifier.getInfo) g)

loadState :: (Gaming g,Read g) => IORef (Engine g) -> IO ()
loadState _en = do
    en@(Engine {game=g}) <- readIORef _en
    case savingStates en of
        Nothing   -> return ()
        Just this -> g `loadFrom` (this ++ "." ++ (identifier.getInfo) g) >>= \r-> writeIORef _en en {game=r}

{-
engineJoystick jb (GLUT.JoystickPosition x y _) = do
    en@(Engine {input=i}) <- getEngine
    when (isRunning en) $ let i' = i {
           left = x < 0,
          right = x > 0,
             up = y > 0,
           down = y < 0,
        button1 = GLUT.joystickButtonA jb == GLUT.Down,
        button2 = GLUT.joystickButtonB jb == GLUT.Down,
        button3 = GLUT.joystickButtonC jb == GLUT.Down
        } in putEngine en {input=inputWithVector i'}
-}

keyboardMouse :: Gaming g => IORef (Engine g) -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
keyboardMouse _en key GLUT.Up   modifiers p = runInputEvent _en (gInputUp   key modifiers) p
keyboardMouse _en key GLUT.Down modifiers p = runInputEvent _en (gInputDown key modifiers) p

setupCursor :: Gaming g => GLUT.Position -> Engine g -> Engine g
setupCursor (GLUT.Position px py) en@(Engine {game=g,inputState=Input {cursor=c,screenCursor=s}}) =

    let sp = translateScreenPos (px,py)          in
    let cp = (camera$getInfo g) `translateCameraPos` sp in
--    let  p = cp `snapToGrid` (gridSize g)       in

    let p = case Just 16 of  -- (cursorGrid$getInfo g) of
              Nothing -> cp
              Just sz -> cp `snapToGrid` sz in

    let i = (inputState en) {
        cursor=p, lastCursor=c,
        delta = p - (lastCursor$inputState en),
        screenCursor=sp, lastScreenCursor=s,
        screenDelta = sp - (lastScreenCursor$inputState en)
        } in

    en {inputState=i, game = (game en) `setInput` i}

runInputEvent :: Gaming g => IORef (Engine g) -> Game g () -> GLUT.Position -> IO ()
runInputEvent _en st p = do
    modifyIORef _en $ setupCursor p

    en <- readIORef _en
    g <- execStateT st (game en)
    writeIORef _en en {game=g}

    modifyIORef _en $ setupCursor p

{-
    getEditor >>= execStateT (setupCursor p >> st >> setupCursor p) >>= \ed->
    when (needsDisplay ed) (GLUT.postRedisplay Nothing) >>
    putEditor ed {needsDisplay = False}
-}

display :: Gaming g => IORef (Engine g) -> IO ()
display _en =
    ioEngine _en $ \en@(Engine {game=g}) ->
      do startFrame (gameWidth$getInfo g) (gameHeight$getInfo g) (gameBackground$getInfo g)
         (a,s) <- runStateT gRender g
         composeDrawing a
         endFrame
         step en {game=s}

uninstall :: (Show g,Gaming g) => IORef (Engine g) -> IO ()
uninstall _en = do --modifyIfInstall _en False $ do
    saveState _en

-- NEWTIME!
--    GLUT.get currentContext >>= justM suspendContext
    --GLUT.idleCallback $= Nothing

    GLUT.keyboardMouseCallback $= Nothing
    GLUT.displayCallback       $= return ()
    GLUT.motionCallback        $= Nothing
    GLUT.passiveMotionCallback $= Nothing

    GLUT.postRedisplay Nothing
    modifyIORef _en $ \en -> en {isInstalled=False}

--modifyIfInstall :: IORef (Engine g) -> Bool -> (Engine g -> IO (Engine g)) -> IO ()
--modifyIfInstall _en b f = readIORef _en >>= \en-> case isInstalled ed of { b -> f en; _ -> return en } >>= _en

install :: (Read g,Gaming g) => IORef (Engine g) -> IO ()
install _en = do --modifyIfInstall _en True $ do
    loadState _en

-- NEWTIME!
--    (GLUT.get currentContext) >>= justM processContext
    --GLUT.idleCallback $= Just (yield >> GLUT.postRedisplay Nothing)

    GLUT.keyboardMouseCallback $= Just (keyboardMouse _en)
    GLUT.displayCallback       $= display _en
    GLUT.motionCallback        $= Just (runInputEvent _en gMouseDragged)
    GLUT.passiveMotionCallback $= Just (runInputEvent _en gMouseMoved)

    t <- GLUT.get GLUT.elapsedTime
    modifyIORef _en $ \en -> en {isInstalled=True, lastCPUTime=t, inputState=noInput}

waitForEvents :: Gaming g => IORef (Engine g) -> Bool -> IO ()
waitForEvents _en b = do
    en@(Engine {waitingEvents=b}) <- readIORef _en
    GLUT.idleCallback $= case b of
                           True  -> Nothing
                           False -> Just (yield >> GLUT.postRedisplay Nothing)
    writeIORef _en en {waitingEvents = not b}
