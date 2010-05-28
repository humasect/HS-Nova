module Editor.Editor (
    Editor(..),newEditor,
    Selection,BrowsingType(..)
) where

import Data.Bits        (complement)
import Control.Monad.State.Lazy
import Data.IORef

import Nova.VectorOps
import Nova.Part
import Nova.Engine
import Nova.Game

data Editor =
    Editor {
           gameInfo :: GameInfo,
              input :: Input,
               name :: String,
              parts :: [Part],

             making :: Maybe Part,
           withPerf :: Performance,
    resourcePreview :: String,

           selected :: [Selection],
           cutStart :: Maybe V2,
        cursorError :: Scalar,
           gridSize :: Scalar,
             border :: Maybe V2,
          mouseDown :: Bool,
          isCloning :: Bool,
      showingLayers :: Int, currentLayer :: Int,
    whiteBackground :: Bool,
       needsDisplay :: Bool,
           showFill :: Bool,
           showWire :: Bool,
         showModels :: Bool,

         inspecting :: Bool,
           browsing :: Bool,
       browsingType :: BrowsingType,
        browsingNum :: Int,
        browsingPat :: String,

             editUV :: Bool,
            seconds :: Scalar,
          isRunning :: Bool
    }
  deriving (Show,Read)

data BrowsingType = BRDicts | BRPalet | BRResources
  deriving (Show,Read,Eq)

type Selection = (Part,[Int])

newEditor = Editor {
           gameInfo = GameInfo {
                        identifier = "edit",
                        gameWidth = 1024,
                        gameHeight = 640,
                        gameBackground = Just (0,0,0,0), --if whiteBackground ed then (1,1,1,0) else (0,0,0,0)
                        camera = (0,0,1)
                       },
              input = noInput,

               name = "temp",     -- /Art/<name>.model
              parts = [],

             making = Nothing,
           withPerf = Visual [Render Default],
    resourcePreview = "empty.nvi",

           selected = [],
        cursorError = 16,
           cutStart = Nothing,
           gridSize = 16.0,
             border = Nothing,
          mouseDown = False,
          isCloning = False,
      showingLayers = complement 0, currentLayer = 0,
    whiteBackground = False,
       needsDisplay = False,
           showFill = True,
           showWire = True,
         showModels = True,

         inspecting = False,
           browsing = False,
       browsingType = BRDicts, browsingNum = 0,
        browsingPat = "",

             editUV = False,
            seconds = 0,
          isRunning = False
    }

