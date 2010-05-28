module Editor.Display (
    render
) where

import Data.List                (sort)
import Data.IORef               (readIORef,writeIORef)
import Control.Monad.State.Lazy (evalStateT)

import Common
import Nova.VectorOps (vecOf,magnitudeOf)
import Nova.HumaMath
import Nova.Draw
import Nova.Resource
import Nova.Winding
import Nova.Engine
import Nova.Part
import Nova.Game -- (GameInfo(..),Input(..))

import qualified Nova.GL as GL

import Editor.Operations
import Editor.Editor
import Editor.UI                (drawPanel)
import Editor.Inspector
import Editor.Browser
import Editor.Events            (isInspectingMany,isInspectingSingle)

renderEditor :: Editor -> Maybe Resource -> [Drawing]
renderEditor ed@(Editor {cursorError=z,input=Input {cursor=c@(x,y)}}) rcts = [
    PointSize 6,

    case making ed of
        Nothing -> Grid $ gridSize ed
        Just  j -> case rcts of
                     Just j@(RImage {}) -> List [Color4 (1,1,1,0.33), ImageRepeated 6 j c]
                     Nothing            -> NoDraw,

    draw $ for (sort $ layerParts ed) unselectedPart,
    draw $ for (sort $ selParts ed) selectedPart,

    PointSize 11,
    draw $ (asVertices.selSteps) ed,
    LineWidth 4,
    case selSteps ed of
        [] -> NoDraw
        ss -> RenderProc (StepVecs 4) $ emptyPart {winding=ss},
    LineWidth 1,

    draw (fmap (\(w,h) -> [
        Color3 (1,0.75,0.75),
        LineBoxAt (w/2,h/2) (0,0),
        LineBoxAt ((w/2)+4,(h/2)+4) (0,0)]
        ) $ border ed),

    -- making a polygon
    draw (fmap (\j -> makingPart $ appendStep (gridSize ed) j c) $ making ed),

    draw cutLine,

    -- cursor
    case mouseDown ed of
      True  -> NoDraw
      False -> case making ed of
                 Nothing -> case rcts of
                              Just j@(RImage {})        -> List [Color4 (1,1,1,0.25), ImageRepeated 1 j c]
                              Just (RModel {rParts=ps}) -> SmallerDo c 0.25 $ List [
                            -- this is pretty much the same code in Render.hs , please fix and clean up after yourself.
                                                               Translate (x,y),
                                                             --drawFor ps renderPart,
                                                               undefined]
                              Nothing                   -> NoDraw
                            --Just                    w -> render w
                 _       -> NoDraw,
--               Proc (ResPreview $ resourcePreview ed) (emptyPart {steps=[Step (0,0) (x,y) (1,1,1,1)]}),

    drawWhen (isRunning ed) animState,

    Color4 (0,1,1,0.75),
    Crosshair $ ((cursor.input) ed,cursorError ed)
    ]
    where
    unselectedPart p = [
        if whiteBackground ed then Color3 (0,0,0) else Color3 (1,1,1),
        if showFill ed then RenderPart p else NoDraw,
        if showWire ed then RenderProc WireColored p else NoDraw]

    makingPart p = [
        RenderPart p,
        Color3 (0.2,0.2,1),
        LineWidth 3,
        RenderProc Wired p,
        LineWidth 1
        ]

    selectedPart p = [
        RenderPart p,
        Color3 (1,0,0),
        LineWidth 2,
        RenderProc Wired p,
        if whiteBackground ed then Color3 (0,0,1) else Color3 (1,1,0),
        RenderProc Bounds p,
        LineWidth 1]

    cutLine = case cutStart ed of
        Nothing -> []
        Just  a -> let b = (cursor.input) ed; sz = cursorError ed in [
            Color3 (1,0,1),
            LineBurst 5 (a,sz),
            drawLines [(a,b)],
            LineBurst 3 (b,sz),
            TextAt ((a + b) / vecOf 2) $ show (magnitudeOf (b - a))]

    animState = let (_,r) = (properFraction.seconds) ed in [
        Color4 (1,0,0,0.5),
        Begin GL.TriangleFan, draw $ ((x,y) : (c,24) `plotCircleSteps` (truncate (360*r),360)), End,

      --pushAttrib (toConst CurrentBit .|. toConst PointBit)
        Color4 (1,0,0,0.75),
      --enable & PointSmooth
        PointSize 16,
        draw $ map (RenderProc TravelKnot) $ allParts ed
      --popAttrib
        ]

render :: Game Editor Drawing
render = do
    ed <- get
    let (x,y) = (cursor.input) ed

    --let rp = resourcePreview ed
    --r <- if null rp then return Nothing else findResource resMap rp >>= return.Just

    d <- drawPanel browserPanel $ (screenCursor.input) ed

    return $ List [
        CameraDo (camera$gameInfo ed) (draw $ renderEditor ed Nothing),
        Color3 (1,1,1),
        if inspecting ed then NoDraw else
            TextAt (-498,-304) ("x: " ++ (show $ truncate x) ++ " y: " ++ (show $ truncate y)),

        --drawInspectors (inspecting ed) $ (screenCursor.input) ed,
        drawWhen (browsing ed) d
        ]

{-    where
    drawInspectors False sc = NoDraw
    drawInspectors  True sc = do
        drawPanel modelInspectorPanel sc
        whenM (isInspectingSingle selParts) $ drawPanel partInspectorPanel     sc
        whenM (isInspectingMany   selParts) $ drawPanel manypartInspectorPanel sc
        whenM (isInspectingSingle selSteps) $ drawPanel stepInspectorPanel     sc -}

{-
listen :: Editor -> Game Editor ()
listen ed = do
    let drawInspectors sc = do
        drawPanel modelInspectorPanel sc
        whenM (isInspectingSingle selParts) $ drawPanel partInspectorPanel     sc
        whenM (isInspectingMany   selParts) $ drawPanel manypartInspectorPanel sc
        whenM (isInspectingSingle selSteps) $ drawPanel stepInspectorPanel     sc

     -- when (inspecting ed) $ evalStateT         (drawInspectors $ (screenCursor.input) ed) ed
     -- when (browsing   ed) $ evalStateT (drawPanel browserPanel $ (screenCursor.input) ed) ed

    when (inspecting ed) $ drawInspectors ((screenCursor.input) ed)
    when (browsing   ed) $ drawPanel browserPanel $ (screenCursor.input) ed
    
    -}