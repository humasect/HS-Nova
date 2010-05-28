module Editor.Events where

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=),Modifiers(..),KeyState(..),Key(..),SpecialKey(..),MouseButton(..))

import Data.IORef
import System.IO.Unsafe   (unsafePerformIO)
import Control.Concurrent (forkIO,threadDelay)
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List          (sort,partition)
import System.Exit

import Common
import Nova.Winding
import Nova.VectorOps (vecOf,snapToGrid)
import Nova.HumaMath
import Nova.Part
import Nova.Mutator
import Nova.Engine
import Nova.Draw
import Nova.Game

import Editor.Editor
import Editor.Operations
import Editor.Inspector
import Editor.Browser
import Editor.UI (isCursorInPanel,panelInput)

{-# NOINLINE dcRef #-}
dcRef :: IORef Bool
dcRef = unsafePerformIO $ newIORef False

watchDoubleClicking = do writeIORef dcRef True
                         forkIO (threadDelay 250000 >> modifyIORef dcRef (\_ -> False))

leftButton :: GLUT.Modifiers -> Bool -> Game Editor ()
leftButton modifiers doubleClicking =
    get >>= \ed ->
    gets snapCursor >>= \v ->
    let c = (v,cursorError ed) in

    if (isJust $ making ed) then addVertex v else
        if doubleClicking then startPolygon v else
            if (alt modifiers == GLUT.Down) then modify (\ed->ed{cutStart=Just v}) else do
                let   ps = reverse $ sort $ (selParts ed) ++ layerParts ed
                let test = if showFill ed then (\s p->
                      if   isConcave p     then s `isCircleInEdges` p else s `isCircleInSteps` p || s `isCircleInside` p)
                      else isCircleInEdges
                let (hit, miss) = (\(Part {winding=w}) -> c `test` w) `partition` ps
                let (sel,unsel) = case shift modifiers of
                                    GLUT.Down -> (hit,miss)
                                    GLUT.Up   -> if not$null hit then ([head hit],miss++(tail hit)) else (hit,miss)

                let finsel = sel `stepIndexesWith` (\(Step _ v _) -> v `isPointInCircle` c)

                liftIO watchDoubleClicking
                modify $ \ed-> ed {mouseDown=True, selected=finsel, parts=unsel++unlayerParts ed}

    where

    startPolygon v =
        get >>= \ed->
        let t = (length $ parts ed) + (length $ selected ed) + 1 in
        let p = appendStep (gridSize ed) (emptyPart {perf=withPerf ed,layer=currentLayer ed}) v in
        put $ ed {making=Just p}

    addVertex v =
        get >>= \ed->
        let p = appendStep (gridSize ed) (fromJust $ making ed) v in
        put $ ed {making=Just p}

rightButton :: GLUT.Modifiers -> Game Editor ()
rightButton modifiers =
    gets making >>= justM (finishPart $ shift modifiers == Down)
    where
    finishPart :: Bool -> Part -> Game Editor ()
    finishPart forSelecting p =
        gets selected >>=   \sel ->
        gets gridSize >>=   \gsz ->

        get >>= \ed ->
        gets snapCursor >>= \c ->

        let m@(Part {winding=w}) = appendStep gsz p c in
        -- reject it if it is less than 3 elements or facing the wrong way.
        let sps = if (length w) > 2 && isConvex w
                  then (m,[{-sz-1-}]) : sel
                  -- try to reverse it and see if it was just drawn clockwise instead of CCW.
                  else let mcp = m {winding=Invert `mutate` (winding m)} in
                                 if isConvex $ winding mcp
                                 then (mcp,[{-sz-1-}]) : sel
                                 else sel
        in
        modify (\ed->ed{making=Nothing}) >>
        case forSelecting of
            False -> modify (\ed-> ed {selected=sps})
            True  -> let (s,_) = head sps in selectWithPart s

inputDown :: Key -> Modifiers -> Game Editor ()
inputDown key modifiers =
    unlessM (key `editorPanelInput` modifiers) $ modify (\ed->ed{editUV=(ctrl$modifiers)==Down}) >> liftIO dirty >>
      case key of
        Char '\ESC' -> clearSelection >> modify (\ed-> ed{making=Nothing})
        Char '\DEL' -> deleteSelected
        Char '\b'   -> deleteSelected
        SpecialKey  KeyLeft -> if shiftDown then moveContents ( 1, 0) else moveCam (-32,  0)
        SpecialKey KeyRight -> if shiftDown then moveContents (-1, 0) else moveCam ( 32,  0)
        SpecialKey    KeyUp -> if shiftDown then moveContents ( 0,-1) else moveCam (  0, 32)
        SpecialKey  KeyDown -> if shiftDown then moveContents ( 0, 1) else moveCam (  0,-32)
        Char 'z' -> toggleFill
        Char 'w' -> toggleWire
        Char '-' -> zoomIn
        Char '=' -> zoomOut
        Char '0' -> modCamera $ \(x,y,_) -> (x,y,1)
        Char '{' -> resizeGrid smaller
        Char '}' -> resizeGrid bigger
        Char '[' -> resizeCursor smaller
        Char ']' -> resizeCursor bigger
        MouseButton   WheelUp -> (if shiftDown then resizeGrid else resizeCursor) smaller
        MouseButton WheelDown -> (if shiftDown then resizeGrid else resizeCursor) bigger

        Char ' ' -> if shiftDown then cloneSelected else modify (\ed->ed {isCloning=True}) -- clone or pick up

        -- in texture mode, rotate texcoords /w /h
        Char ',' -> mutateSel (CenterRotate    15)
        Char '.' -> mutateSel (CenterRotate (-15))
        Char '<' -> gets snapCursor >>= \cur -> mutateSel (RotateAround cur    15)
        Char '>' -> gets snapCursor >>= \cur -> mutateSel (RotateAround cur (-15))

      --Char '<' -> liftIO (readIORef resMap) >>= \rm -> modSelWindings (rotateContentsBy rm (-5))
      --Char '>' -> liftIO (readIORef resMap) >>= \rm -> modSelWindings (rotateContentsBy rm 5)
      --Char 't' -> modSelWindings (mutateList [Rotate 15,AddXY(32,32),AutoCurve 4])

      --Char 'f' -> modSelParts (\p -> p {   isFan=not$isFan p   })
      --Char 'c' -> modSelParts (\p -> p {isCurved=not$isCurved p})

        Char 'i' -> mutateSel Invert
        Char 'v' -> mutateSel RotateStepsL
        Char 'V' -> mutateSel RotateStepsR
      --Char 'V' -> modSelParts (\p -> p {winding=transpose.reverse $ winding p})

        MouseButton  LeftButton ->
            liftIO (readIORef dcRef) >>= \dc-> modify(\ed->ed{isCloning=False}) >> leftButton modifiers dc
        MouseButton RightButton -> rightButton modifiers
          --gets making >>= \m -> if isNothing (liftIO $ putStrLn "Menu!") (finishWinding shiftDown) m

        Char 'r' -> modColors (-0.1,0,0,0)
        Char 'R' -> modColors ( 0.1,0,0,0)
        Char 'g' -> modColors (0,-0.1,0,0)
        Char 'G' -> modColors (0, 0.1,0,0)
        Char 'b' -> modColors (0,0,-0.1,0)
        Char 'B' -> modColors (0,0, 0.1,0)
        Char 'a' -> modColors (0,0,0,-0.1)
        Char 'A' -> modColors (0,0,0, 0.1)

{-
        Char 'p' -> gets isRunning >>= \b -> liftIO $ do
            -- similar in Engine.hs
            case b of
                True -> do
                    uninstall edEngine
                  --GLUT.keyboardMouseCallback $= Just keyboardMouse
                  --GLUT.motionCallback        $= Just (runInputEvent mouseDragged)
                  --GLUT.passiveMotionCallback $= Just (runInputEvent   mouseMoved)
                False -> do
                    install edEngine
                  --GLUT.keyboardMouseCallback $= Just (Engine.keyboardMouse edEngine)
                  --GLUT.passiveMotionCallback $= Just (Engine.anyMotion     edEngine)
                  --GLUT.motionCallback        $= Just (Engine.anyMotion     edEngine)
-}

        Char '\t' -> gets (screenCursor.input) >>= \(x,_) -> if x < 0
                then modify $ \ed->ed {inspecting=(not.inspecting) ed}
                else modify $ \ed->ed {  browsing=  (not.browsing) ed}

        Char 'q'  -> liftIO $ exitFailure
        Char '`'  -> get >>= liftIO . print
      --Char x    -> liftIO (putStrLn [x])
        _         -> return ()

    where
    shiftDown = shift modifiers == Down
    moveCam v = modCamera (\(x,y,z) -> let (cx,cy) = (x,y)+v in (cx,cy,z))
--moveCam v = modify $ \ed@(Editor {camera=(x,y,z)}) -> let (cx,cy) = (x,y)+v in ed{camera=(cx,cy,z)}
  --moveContents v = liftIO (readIORef resMap) >>= \rm -> modSelWindings (moveContentsBy rm v)
    moveContents v = return ()

    zoomIn  = modCamera (\(x,y,z) -> (x,y,if z < 0.1 then z else (z*0.75)))
    zoomOut = modCamera (\(x,y,z) -> (x,y,if z >  10 then z else (z*1.25)))
    modColors cm = modSelSteps (\(Step t v c) -> Step t v (cm + c))

    cloneSelected = modify $ \ed-> let copy = selected ed in
                                   ed {isCloning=True, parts=(selParts ed)++(parts ed), selected=copy}

    smaller x = if x>  1 then x/2 else x
    bigger  x = if x<128 then x*2 else x
    resizeGrid   f = modify $ \ed@(Editor{gridSize=gs   }) -> ed {   gridSize = f gs}
    resizeCursor f = modify $ \ed@(Editor{cursorError=cs}) -> ed {cursorError = f cs}

inputUp :: GLUT.Key -> GLUT.Modifiers -> Game Editor ()
inputUp key modifiers =
    gets cutStart >>=   \cs ->
    getLips       >>= \lips ->
    if (not$or lips) && (key == MouseButton LeftButton) && (isJust cs) then
        cutSelected >> liftIO dirty >> modify (\ed-> ed{cutStart=Nothing})
        else when (key == MouseButton LeftButton) $ modify (\ed-> ed{mouseDown=False}) >>

    modify (\ed->ed{editUV=(ctrl$modifiers)==Down})

--
-- these two below share many similarities.
--

mouseDragged :: Game Editor ()
mouseDragged = do
    liftIO dirty
  --getLips >>= \lips ->
    {-(not$or lips) && -}
    gets cutStart >>= nothingM (
        get >>= \ed->

        let              cdelta = (cursor.input) ed - (lastCursor.input) ed in
        let xyDrag (Step t v c) = let n = v + cdelta in
            -- if dragging whole windings, dont snap individual steps to grid points. otherwise do.
                                  Step t (if null$selSteps ed then n else n `snapToGrid` (gridSize ed)) c in

        let              sdelta = (screenCursor.input) ed - (lastScreenCursor.input) ed in
        let uvDrag (Step t v c) = let d = sdelta / (vecOf$gridSize ed) in let (nx,ny) = (t + d) in Step (nx,ny) v c in

        whenM2 (gets editUV) (modSelSteps uvDrag) (modSelSteps xyDrag)
        )

mouseMoved :: Game Editor ()
mouseMoved = do
    liftIO dirty
    whenM (gets isCloning) $
        get >>= \ed ->
        let            cdelta = (cursor.input) ed - (lastCursor.input) ed in
        let drag (Step t v c) = Step t (v + cdelta) c in

        modSelSteps drag

--- here are some twisted methods..

isInspectingMany   = liftGets $ \sw-> (not.null) sw
isInspectingSingle = liftGets $ \sw-> (not.null) sw && length sw < 2

getLips :: Game Editor [Bool]
-- the most horrible function i have seen. how shameful.
getLips = do
    ed <- get
    let sc = (screenCursor.input) ed
    --let ok = (isNothing.making) ed && (not.isCloning) ed
    let ok = liftM2 (&&) (isNothing.making) (not.isCloning) ed

    let bb       = ok && browsing   ed && sc `isCursorInPanel` browserPanel
    let ib       = ok && inspecting ed && sc `isCursorInPanel` modelInspectorPanel

    let chk x ip = ok && inspecting ed && ip && sc `isCursorInPanel` x

    wb  <- liftM (chk     partInspectorPanel) $ isInspectingSingle selParts
    mpb <- liftM (chk manypartInspectorPanel) $ isInspectingMany   selParts
    sb  <- liftM (chk     stepInspectorPanel) $ isInspectingSingle selSteps

    return [bb,ib,wb,sb,mpb]

editorPanelInput :: Key -> Modifiers -> Game Editor Bool
-- second most horrible function.
editorPanelInput key modifiers = do
    lips <- getLips
    sc  <- gets (screenCursor.input)

    let bb  = lips !! 0
    let ib  = lips !! 1
    let wb  = lips !! 2
    let sb  = lips !! 3
    let mpb = lips !! 4

    let pi = panelInput sc key modifiers

    let setBrowsing   keep = modify $ \ed -> ed {browsing   = keep}
    let setInspecting keep = modify $ \ed -> ed {inspecting = keep}
    
    when bb  $ pi           browserPanel >>= setBrowsing

    when ib  $ pi    modelInspectorPanel >>= setInspecting

    when wb  $ pi     partInspectorPanel >>= setInspecting

    when sb  $ pi     stepInspectorPanel >>= setInspecting

    when mpb $ pi manypartInspectorPanel >>= setInspecting

    if or lips then liftIO dirty >> return True else return False
