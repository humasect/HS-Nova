{-# LANGUAGE FlexibleContexts #-}

module Editor.Inspector (
    modelInspectorPanel,
    partInspectorPanel,
    stepInspectorPanel,
    manypartInspectorPanel
) where

import Data.Maybe       (isNothing)
import Data.IORef
import Control.Monad.State.Lazy
import Data.Bits
import Data.Maybe       (isJust)
import Data.Ratio

import Config           (rootDir)
import Common
import Nova.HumaMath
import Nova.Render
import Nova.Winding
import Nova.Part
import Nova.Mutator
import Nova.Game
import Nova.Engine

import qualified Nova.GL as GL
import Nova.Draw

import Editor.Operations
import Editor.UI
import Editor.Editor

modelInspectorPanel = Panel {
    bounding = ((256,212),(-512,108)), --((128,320),(-384,0)),
    controls = liftM2 (\lbs vbs -> [strName,btnSel,lblGridScale,lblCursor,btnLoadNew,btnSave]++lbs++vbs)
        makeLayerButtons makeViewButtons,
    drawBack = draw [brightColor, drawLines [((0,0),(256,0))] ]
    }
    where

    makeViewButtons = return [
        (newControl SmallButton ((40,16),(130,4)) "Whit"), --{
        -- TODO: fix white background
--              fun = modify $ \ed-> ed {whiteBackground=(not.whiteBackground) ed},
--            state = gets whiteBackground
--        },
        (newControl SmallButton ((40,16),(86,4)) "Fill") {
              fun = toggleFill,
            state = gets showFill
        },
        (newControl SmallButton ((40,16),(44,4)) "Wire") {
              fun = toggleWire,
            state = gets showWire
        },
        (newControl SmallButton ((40,16),(2,4)) "Modl") {
              fun = modify (\ed->ed {showModels=not$showModels ed}),
            state = gets showModels
        }
        ]

    layerButton h x y = (newControl SmallButton ((16,16),(x,y)) "") {
        state = liftM (flip testBit (h+16)) $ gets showingLayers,
        fun = clearSelection >> modify (\ed@(Editor {showingLayers=sl})-> ed {showingLayers=
            if sl `testBit` (h+16) then sl `clearBit` (h+16) else sl `setBit` (h+16)}),
        fun2 = modSelParts (\p -> p {layer = h}) >>
            modify (\ed@(Editor {currentLayer=cl})-> ed {currentLayer = h}),
        label = liftGets2 (\lps cl-> if cl==h then "C" else if (not.null) lps then "p" else "")
            (partsInLayer h) currentLayer
        }

    makeLayerButtons = do
        let col x y h = return $ layerButton h x y
        a <- refLoop (col 234) (164,-16) [-8,-6..8]
        b <- refLoop (col 216) (156,-16) [-7,-5..7]
        return $ a ++ b ++ [toph,bottomh]
        where
        toph    = (newControl Label ((0,0),(217,172.333)) "-") {state=return True}
        bottomh = (newControl Label ((0,0),(217, 33.333)) "+") {state=return True}

    btnSel = (newControl Button ((80,24),(174,4)) "") {
        fun = invertSelection,
        label = liftM (\ed-> "sel "++(show.length$selParts ed)++"/"++(show.length$parts ed)) get
        }

    lblGridScale = (newControl Label ((0,0),(8,160)) "") {
        label = liftM (\ed@(Editor {gridSize=sz}) ->
            let (_,_,cz) = (camera.gameInfo) ed in
            "grid: " ++ show (round sz) ++ " at " ++ (show $ round $ cz*100) ++ " %") get
        }
    lblCursor = (newControl Label ((0,0),(4,64)) "") {
        label = gets cursorError >>= \e-> liftM (\(x,y) -> "x:"++show x++" y:"++show y++" e:"++(show$round e))
                                                (return (0,0)), --(getInput _engine cursor),
        state = return True
        }

    btnLoadNew = (newControl Button ((48,24),(4,186)) "") {
        fun = existingFileName >>= \b->
            if isNothing b then modify (\ed -> ed{selected=[], parts=[]}) else reloadFile,
        label = liftM (\b-> if isNothing b then "New+" else "Load") existingFileName
        }

    btnSave = (newControl Button ((48,24),(204,186)) "Save") { fun = saveFile }

    strName = (newControl EditLabel ((144,24),(56,186)) "") {
        label = gets name,
        changeLabel = Just $ \s -> modify (\ed->ed {name=s})
        }

manypartInspectorPanel = Panel {
    bounding = ((256,60),(-512,44)),
    controls = return [btnCalcUV,btnSnap,btnSel,btnLayer "Up" (+) 2, btnLayer "Down" (-) 56],
    drawBack = draw [brightColor, drawLines [((0,0),(256,0))] ]
    }

    where
    btnSel = (newControl Button ((80,24),(174,2)) "") {
        fun = invertSelSteps,
        label = liftGets (\s ->
                                let ns = sum$map (\(_,ss) ->          length ss) s in
                                let ts = sum$map (\(p, _) -> (length.winding) p) s in
                                "sel "++(show ns)++"/"++(show ts))
                         selected
        }

    btnLayer s f x = (newControl Button ((48,24),(x,2)) s) {
        fun = modSelParts $ \p-> p {layer=f (layer p) 1}
        }

    btnSnap = (newControl Button ((48,24),(72,32)) "Snap") {
        fun = gets gridSize >>= \sz-> mutateSel (SnapToGrid sz)
        }

    btnCalcUV = (newControl Button ((64,24),(4,32)) "CalcUV") {
        --fun = modSelWindings (\w-> mapM (calcStepUV w) (steps w) >>= \ss-> return w {steps=ss}),
        fun = get >>= \ed->
            --forM (selected ed) (\(w,s) -> liftIO (calcPartUV (gridSize ed) w) >>= \w' -> return (w',s)) >>= \sps->
              put ed -- {selected=sps}
        }

partInspectorPanel = Panel {
    bounding = ((256,284),(-512,-236)),
    controls = makeContentList >>= \cl-> return $ [lblFlag,lblFlagE,btnStore,btnFreeze]++cl,
    drawBack = draw [brightColor, drawLines [((0,0),(256,0))] ]
    }
    where

    btnStore  = (newControl Button ((56,24),(4,4)) "Store")

    btnFreeze = (newControl Button ((56,24),(64,4)) "Freeze")

    lblFlag  = (newControl     Label ((0,0),(4,256)) "flags=")
    lblFlagE = (newControl EditLabel ((184,16),(56,256)) "") {
        state = return True,
        label = liftGets (flags.head) selParts,
        changeLabel = Just $ \s -> modSelParts (\p-> p{flags=s})
        }

    makeContentList = return []

{-
    do
        ypos <- liftIO $ newIORef 32
        sp <- gets (head.selParts)
        mapM (\(t,n)-> do
            y <- liftIO $ readIORef ypos
            let c = (newControl ClickLabel ((192,12),(8,y)) "") {
                label = return $ show n++": "++show t,
                fun2 = modSelParts (\p-> p {perf=deleteIndex n $ contents p}),
                state = return False }
            liftIO $ writeIORef ypos (y+12)
            return c) $ zip (perf sp) [0..(length$perf sp)-1]
-}

--stepInspectorPanel :: (MonadIO m, MonadState Document m) => Panel m
stepInspectorPanel = Panel {
    bounding = ((256,80),(-512,-320)),
    controls = do
        left  <- makeStepLabels "UVRG" [uOf,vOf,rOf,gOf]  16
        right <- makeStepLabels "XYBA" [xOf,yOf,bOf,aOf] 136
        return $ left++right,
    drawBack = NoDraw
    }
    where
    --readVal x orig = liftIO $ catch (readIO x) (\e -> print e >> return orig)
    --uSet u (Step (_,v) vv cc) = Step (u,v) vv cc

    uOf (Step (u,_) _ _) = u
    vOf (Step (_,v) _ _) = v
    xOf (Step _ (x,_) _) = x
    yOf (Step _ (_,y) _) = y
    rOf (Step _ _ (r,_,_,_)) = r
    gOf (Step _ _ (_,g,_,_)) = g
    bOf (Step _ _ (_,_,b,_)) = b
    aOf (Step _ _ (_,_,_,a)) = a

    --makeStepLabels :: (MonadState s m) => [Char] -> [(Step -> Scalar)] -> Scalar -> Editor [Control m]
    makeStepLabels a b xp = do
        ns <- refLoop (\y h -> return $ newControl Label (  (0,0),   (xp,y)) [h]) (56,-16) a
        let l y (n,ns) = return $ (newControl Label ((96,16),(xp+16,y))  "") {
                                  label = liftGets (\z -> show $ approxRational (ns $ head z) 0.001) selSteps,
                                  state = return True :: Game Editor Bool }
        es <- refLoop l (56,-16) $ zip a b
        return $ ns ++ es

          --changeLabel = Just (\l -> readVal l 0 >>= \z -> modSelSteps (uSet z)),
          --fun = modSelSteps (\(Step t v (r,g,b,a)) -> Step t v (if r<1 then r+0.05 else 1,g,b,a)),
          --fun2 = modSelSteps (\(Step t v (r,g,b,a)) -> Step t v (if r>0 then r-0.05 else 0,g,b,a)),
    --lblU = (newControl Label ((128,16),(4,4)) "U")

