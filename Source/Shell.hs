module           Shell where

import           GHC
import           MonadUtils
import           SrcLoc

import qualified Outputable
import           System.IO
import           Data.Maybe

import           Graphics.UI.GLUT                 hiding (screenSize)
import           Graphics.Rendering.OpenGL
import           Sound.OpenAL.ALC                 as ALC

import           Data.IORef
import           System.IO.Unsafe
import           System.Exit
import qualified System.Console.Editline.Readline as Readline
import           Control.Concurrent                      (forkIO)
import           DynFlags                                (defaultDynFlags,DynFlags(..))
import           Control.Exception
import           Control.Monad                           (when)
import           Network

import Boot
import Config
import Common

data Shell = Shell {
    addModules :: [String],
    saveStates ::  Bool,
    device     ::  Maybe ALC.Device,

    bootMode   ::  BootMode
  }

shell :: IORef Shell
shell = unsafePerformIO $ newIORef
            Shell {
               addModules = ["Dev"],
               saveStates = True,

               device     = Nothing,

               bootMode   = Play
            }

getShell    = readIORef shell
putShell    = writeIORef shell
modifyShell = modifyIORef shell

shellThread :: Ghc ()
shellThread = do
    sh@(Shell {saveStates=ss}) <- liftIO getShell

    -- ask for a Haskell expression to evaluate
    stmt                       <- liftIO $ Readline.readline $ "nova@" ++ (show $ addModules sh) ++ "> "

    case stmt of
      Nothing -> liftIO $ putStrLn "Premature shell exit." >> exitWith ExitSuccess
      Just j  -> liftIO (Readline.addHistory j) >>
                 case j of
                   ""             -> liftIO $ putStrLn "* Next event."
                 --":q"           -> destroyWindow (fromJust $ window sh) >> exitWith ExitSuccess
                   ":ss"          -> liftIO $ putShell sh {saveStates=not ss} >>
                                              putStrLn ("* Save States " ++ if not ss then "ON." else "OFF")
                   ":q"           -> when ss (execStmt $ (bmModuleName.bootMode) sh++".shutdown True")
                                       >> liftIO exitSuccess
                                                                               -- throwIO (ExitException ExitSuccess)
                   ":re"          -> when ss (execStmt $ (bmModuleName.bootMode) sh++".shutdown True") >> reloadModules []
                   ':':'m':' ':xs -> reloadModules [xs]
                 --':':'m':' ':xs -> loadModule session xs

                   st             -> execStmt st

    liftIO $ postRedisplay Nothing
    shellThread

execStmt      :: String -> Ghc ()
execStmt stmt = do result <- GHC.runStmt stmt GHC.RunToCompletion  -- not SingleStep
                   case result of
                     GHC.RunOk _        -> return () -- putStrLn $ "* Ok: " ++ showNames names -- see below
                     GHC.RunFailed      -> liftIO $ putStrLn "* Failed"
                   --GHC.RunException (ExitException code) -> exitWith code
                     GHC.RunException e -> liftIO $ putStrLn ("* Exception: " ++ show e)
                     GHC.RunBreak _ _ _ -> liftIO $ putStrLn "* Break."	
    --where
    --showNames :: [GHC.Name] -> String
    --showNames = Outputable.showSDoc . Outputable.ppr

{-
loadModule :: GHC.Session -> String -> IO ()
loadModule session m = do
    let mn = GHC.mkModuleName m
    GHC.addTarget session =<< (GHC.guessTarget m Nothing)
    df <- GHC.getSessionDynFlags session
    ok <- GHC.defaultCleanupHandler df (GHC.load session (GHC.LoadUpTo mn))

    case ok of
        GHC.Succeeded -> putStrLn "* Module load success." >> finishLoad
        GHC.Failed -> putStrLn "* Module load failed."
    where
    finishLoad = do
        context <- mapM (\m -> GHC.findModule session (GHC.mkModuleName m) Nothing) $
            ["Prelude", "Dev"] ++ [m]
        GHC.setContext session [] context
        modifyShell (\sh-> sh{curModule=m})
-}

reloadModules         :: [String] -> Ghc ()
reloadModules addMods = do
    --GHC.setTargets session []
    --GHC.load session GHC.LoadAllTargets
    session <- getSession

    sh      <- liftIO getShell
    ts      <- mapM (\n-> GHC.guessTarget n Nothing) $ (bmModuleName.bootMode) sh : addMods
    mapM_ (\t@(GHC.Target id _ _) -> GHC.removeTarget id) ts
    GHC.setTargets ts

    GHC.getSessionDynFlags
  --ok <- GHC.defaultCleanupHandler df (GHC.load session GHC.LoadAllTargets)
    ok <- GHC.load GHC.LoadAllTargets
    case ok of
        GHC.Succeeded -> liftIO  (putStrLn "* Modules load succeeded.") >> finishLoad sh
        GHC.Failed    -> liftIO $ putStrLn "* Modules load failed."

    where
    finishLoad sh = do
        context <- mapM (\m -> GHC.findModule (GHC.mkModuleName m) Nothing)
                     $ ["Prelude", (bmModuleName.bootMode) sh] ++ addMods

        GHC.setContext [] context

      --execStmt $ (bmModuleName.bootMode) sh++".init "++if (saveStates sh) then "True " else "False "++(initArgs sh)
        execStmt $ "Boot.initMode " ++ if saveStates sh then "(Just \"autosave\") " else "Nothing "
                                    ++ (show.bootMode) sh
        liftIO $ putShell sh {addModules=addMods}

initShell :: Ghc ()
initShell = do
    df       <- GHC.getSessionDynFlags
    (f1,_,_) <- GHC.parseDynamicFlags (df {
        GHC.verbosity=1,
        GHC.hscTarget=GHC.HscAsm,
      --GHC.hscTarget=GHC.HscInterpreted,          -- fails with GHC.exception when loading AL.hsc
        GHC.ghcLink=GHC.LinkInMemory}) [x "-hidir ../build", x "-odir ../build", x "-B../build", x "-DCALLCONV=ccall"]
                                                            --"-Wall"]
    GHC.setSessionDynFlags f1

    reloadModules []
    GHC.defaultErrorHandler df shellThread

    where x s = L noSrcSpan s

main :: IO ()
main = boot (Just "autosave") $ Just $ \b -> do
    modifyShell $ \sh-> sh{bootMode=b}
                                        --initArgs=case n of {"Editor"->">> Editor.open "++show arg;_->""}})

    forkIO (runGhc (Just ghcPath) initShell)
    return ()
