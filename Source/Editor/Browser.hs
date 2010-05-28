module Editor.Browser (
    browserPanel
) where

import System.Process
import System.IO
import Text.Regex
import Control.Monad.State.Lazy
import Data.Maybe
import System.Directory
import System.FilePath

import Common
import Config
import Nova.Resource
import Nova.Mutator
import Nova.Part -- for Procedural and PerfStep
import Nova.Synthesizer
import Nova.Draw
import qualified Nova.GL as GL

import Editor.Operations
import Editor.UI
import Editor.Editor

{-

[     *      ] [Prune][Build]
[dicts][palet] [procs][mutes][aural]






-}

btnBuild = (newControl Button ((52,24),(200,608)) "Build") {
    fun = liftIO $ putStrLn "Executing.." >> buildContents >> putStrLn "..Done."
    }
    where
    buildContents = do
        ph <- runProcess "make" ["content"] (Just rootDir) Nothing Nothing Nothing Nothing -- (Just stdin) (Just stdin)
      --follow ph 620
        waitForProcess ph
{-  follow ph y = do
        ln <- hGetLine stdin
        drawTextAt (-320,y) ln
        endFrame
        c <- getProcessExitCode ph
        if isNothing c then follow ph (y+16) else return () -}

btnPrune = (newControl Button ((52,24),(144,608)) "Prune") { fun = pruneResources False }

data BrowsingMode =
      BRVisua |  BRTransf | BRAural
    | BRProcs |   BRMutes | BRSynth
    | BRModel |  BRImages | BRSound
  deriving (Eq)

--  [dicts] [     ] [visua] [transf] [aural]
--  [     ] [palet] [procs] [mutes ] [synth]
--  [     ] [     ] [model] [images] [sound]

browsingModes :: BrowsingType -> [BrowsingMode]
browsingModes     BRDicts = [BRVisua,BRTransf,BRAural]
browsingModes     BRPalet = [BRProcs, BRMutes,BRSynth]
browsingModes BRResources = [BRModel,BRImages,BRSound]

contentsFile :: BrowsingMode -> String
contentsFile  BRVisua =   "visuals.npl"
contentsFile BRTransf = "trasforms.npl"
contentsFile  BRAural =    "aurals.npl"
contentsFile  BRProcs =     "procs.nhs"
contentsFile  BRMutes =     "mutes.nhs"
contentsFile  BRSynth =    "synths.nhs"
contentsFile  BRModel =  "*.nvm.npl"
contentsFile BRImages =  "*.nvi.npl"
contentsFile  BRSound =  "*.nvs.npl"

mkContentsList :: String -> BrowsingType -> Int -> IO [(String,Performance)]
mkContentsList pat brt num = do
    let matches n = isJust $ matchRegex (mkRegex $ "^.*"++pat++".*") n
    let brm = browsingModes brt !! num

    --reloadContentsMaps resMap
    --safeReadScript resMap "asdasd"

    RPerfList ps <- findResource resMap (contentsFile brm)
    return ps
{-
    case contentsFile brm of
        --Left  l -> safeReadScript resMap l
        Left  l -> findResource resMap l
        Right r -> do dirs <- findAllResourceDirs ""
                      ds   <- mapM findAllResNamesInSub dirs
                      return $ (filter (\n -> (takeExtension n == r) && matches n) . concat) ds 
-}

persistBrButtons brt = [
    (newControl SmallButton ((48,24),(  2,580)) "Dicts") {
        state = return (brt==BRDicts),
          fun = modify $ changeBrType BRDicts
    },
    (newControl SmallButton ((48,24),( 52,580)) "Palet") {
        state = return (brt==BRPalet),
          fun = modify $ changeBrType BRPalet
        }
    ]
    where
    changeBrType brt ed = ed { browsingType = 
        case browsingType ed of
            BRResources -> brt
            brt -> BRResources
        }

threeButtons (nm1,nm2,nm3) = [
    btnBrowse 0 ((48,24),(102,580)) nm1,
    btnBrowse 1 ((48,24),(154,580)) nm2,
    btnBrowse 2 ((48,24),(206,580)) nm3
    ]
    where
    btnBrowse num sq l = (newControl SmallButton sq l) {
        state = liftGets ((==) num) browsingNum,
          fun = modify $ \ed-> ed {browsingNum=num}
        }

browsingButtons     brt@BRDicts = persistBrButtons brt ++ threeButtons ("Visua","Transf","Aural")
browsingButtons     brt@BRPalet = persistBrButtons brt ++ threeButtons ("Procs", "Mutes","Synth")
browsingButtons brt@BRResources = persistBrButtons brt ++ threeButtons ("Model","Images","Sound")

browserPanel = Panel {
    bounding = ((256,640), (512-256,-320)),
    controls = do
        pat <- gets browsingPat
        brt <- gets browsingType
        num <- gets browsingNum
        liftM ((++) $ [lblPattern,btnBuild,btnPrune]++browsingButtons brt) (liftIO $ makeItems pat brt num),
    drawBack = draw [ brightColor, Begin GL.Lines, Vertex2 (0,576), Vertex2 (256,576), End ]
    }
    where
    lblPattern = let c = (newControl EditLabel ((72,16),(8,616)) "") in c {
        label = gets browsingPat,
        changeLabel = Just $ \l -> modify (\ed->ed {
            browsingPat = if (length.browsingPat) ed >=8
                          then if length l < (length.browsingPat) ed then l else browsingPat ed
                          else l}),
        state = return True
        }

    makeItems pat brt num = do
        let item y (k,a) = return $ (newControl ClickLabel ((256,12),(0,y)) k) -- {
                                 --    fun = addContents h,
                                 --   fun2 = removeContents h
                                 --state = ...
                               --  }

        ns <- mkContentsList pat brt num
        refLoop item (564,-12) ns
        where

{-
    brExt BRModels = ".nvm"
    brExt BRImages = ".nvi"
    brExt BRSounds = ".nvs"

    brBuild BRModels x = Model x
    brBuild BRImages x = Textured x
    brBuild BRSounds x = Sound x

    -- TODO: move this stuff to Resource.loadScript (safeReadScript) to internalize the Read versions.

    makeItems = do
        brm <- gets browsingMode
        brp <- gets browsingPat
        let matches n = isJust $ matchRegex (mkRegex $ "^.*"++brp++".*") n

--      ns <- liftIO $ contentsForMode matches brm

        let build x a = (liftM x (return a)) :: [Contents]
        let intern x ns = return $ map (\n-> (,) (show n) (build x $ n)) ns
        ns <- liftIO $ case brm of
            --BRLocal -> liftM (filter (\n-> dropExtension n /= "empty" && takeExtension n /= ".nhs" && matches n)
            --    . allResourceNames) (readIORef resMap)
            BRContents -> (safeReadScript resMap "contents.nhs") :: IO [(String,[Contents])]
            BRMutators -> ((safeReadScript resMap "mutes.nhs") :: IO [Mutator]) >>= intern Trans
            BRProcedurals -> ((safeReadScript resMap "procs.nhs") :: IO [Procedural]) >>= intern Proc
            BRSynthesizers -> ((safeReadScript resMap "synths.nhs") :: IO [Synthesizer]) >>= intern Synth
{-          _ -> findAllResourceDirs "" >>= liftM (filter (\n -> (takeExtension n == brExt brm) && matches n) . concat)
                    . (mapM (\n-> liftM (map (\nn-> (,) nn (brBuild brm nn))) findAllResNamesInSub)) -}
            _ -> collectResources >>= intern (brBuild brm)
                where
                collectResources = do
                    dirs <- findAllResourceDirs ""
                    ds <- mapM findAllResNamesInSub dirs
                    return $ (filter (\n -> (takeExtension n == brExt brm) && matches n) . concat) ds

        ypos <- liftIO $ newIORef 564
        mapM (\cpn@(pn,_) -> do
            y <- liftIO $ readIORef ypos
            let c = (newControl ClickLabel ((256,12),(0,y)) pn) {
                fun = addContents cpn
                ,fun2 = removeContents cpn
            --  ,state = liftGets (\c-> show n == showContents c) withContents
            }

            liftIO $ writeIORef ypos (y-12)
            return c) ns

        where
        showContents :: Contents -> String
        showContents (Textured s) = s
        showContents (Image s) = s
        showContents (Model s) = s
        showContents (Sound s) = s
        showContents (Proc p) = show p
        showContents (Synth s) = show s
        showContents (Trans m) = show m
-}
