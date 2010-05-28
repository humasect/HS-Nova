module Boot (
    boot,
    BootMode(..),BootEngine,
    
    -- for Shell
    bmModuleName
) where

-- used by Shell, Main, and BuildContent

import qualified Graphics.UI.GLUT  as GLUT
import           Graphics.UI.GLUT   (($=))
import           Graphics.Rendering.OpenGL
import           Sound.OpenAL.ALC  as ALC
import qualified Control.Exception as E

import Network            (withSocketsDo)

import Config
import Common
import Nova.VectorOps
import Nova.Engine
import Nova.Edit as Edit
import Nova.Play as Play
--import Nova.Orbedit

bmModuleName :: BootMode -> String
bmModuleName bm = "Nova."++show bm

initView:: String -> GLUT.GLsizei -> GLUT.GLsizei -> IO ()
initView title w h = do
    let sz = GLUT.Size w (h+titleBarHeight)

    GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.WithDepthBuffer, GLUT.WithStencilBuffer]
    GLUT.createWindow       title
    GLUT.windowPosition     $= (GLUT.Position 80 40)
    GLUT.windowSize         $= sz
    GLUT.displayCallback    $= return ()
    GLUT.viewport           $= (GLUT.Position 0 0, sz)

startupMode:: IO BootMode
startupMode = GLUT.getArgsAndInitialize >>= \(_,args) -> do
    if (not.null) args && (head args == "-edit" || head args == "-orbedit")
        then initView "Nova Edit" 1024 640 -- >> writeIORef wWidth 1024
        else initView "Nova Play"  640 640 -- >> writeIORef wWidth 640

    return $ case args of
               "-edit":xs    -> (   Edit $stringToMaybe$ concat xs)
               "-orbedit":xs -> (Orbedit $stringToMaybe$ concat xs)
               _             ->  Play

initAudio:: IO ()
initAudio = 
    ALC.openDevice Nothing >>= maybe (putStrLn "** No audio.") initContext
    where
    initContext d =
        get (ALC.deviceSpecifier d) >>= \str -> putStrLn ("** Init audio: '" ++ (show str) ++ "'")
               >> createContext d [Frequency 48000] >>= ($=) currentContext

shutdownAudio :: IO ()
shutdownAudio = do
    c <- get ALC.currentContext
    case c of
        Nothing -> return ()
        Just j  -> do
            d <- get $ contextsDevice j
            justM (\j-> do
                putStrLn "** Shutdown Audio."
                c <- get ALC.currentContext
                justM ALC.destroyContext c
                ALC.closeDevice j
                return ())
                d

initMode :: Maybe String -> BootMode -> IO ()
initMode ss               Play = Play.plEngine `usingStatesNamed` ss >> install Play.plEngine -- >> Play.bootInit b
initMode ss (Edit       model) = Edit.engine `usingStatesNamed` ss >> install Edit.engine -- >> Edit.bootInit b
initMode ss (Orbedit universe) = error "Orbedit not installed"

--bootInit :: BootEngine b => b -> IO ()

boot :: Maybe String -> Maybe (BootMode -> IO ()) -> IO ()
boot ss f = withSocketsDo $ do
    initAudio

    startupMode >>= runFun ss f

    GLUT.mainLoop
  --shutdownAudio

    where
    runFun ss f b = do
        initMode ss b
        case f of
                   Nothing ->  return ()
                   Just  j -> E.catch (j b)
                                      (\(E.SomeException e) -> do shutdownAudio
                                                                  putStrLn $ "finished: " ++ show e)