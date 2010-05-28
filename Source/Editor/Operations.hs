module Editor.Operations where

import Data.Maybe
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BC
import Codec.Compression.GZip
import Data.List  (partition,findIndices,delete)
import Data.Bits  (testBit)
import Data.IORef (readIORef,writeIORef)
import Control.Monad.State.Lazy

import Config     (artDir,contentDir)
import Common
import Nova.Resource
import Nova.VectorOps
import Nova.HumaMath
import Nova.Part
import Nova.Winding
import Nova.Game
import Nova.Engine
import qualified Nova.Mutator     as MU
import qualified Nova.Synthesizer as SY

import Nova.Draw
import qualified Nova.GL          as GL

import Editor.Editor

snapCursor :: Editor -> V2
snapCursor ed = ((\c-> c `snapToGrid` gridSize ed).cursor.input) ed

modCamera :: (V3 -> V3) -> Game Editor ()
modCamera f = modify $ \ed@(Editor {gameInfo=gi}) -> ed { gameInfo = gi { camera = f (camera gi) } }

modSel :: (Selection -> Selection) -> Game Editor ()
modSel f = modify $ \ed-> ed{selected = map f (selected ed)}

modSelSteps :: (Step -> Step) -> Game Editor ()
modSelSteps f = modSel
    (\(p,i) -> if null$i then (p{winding=map f $ winding p},i)
                 else (p{winding=map (\n -> if n `elem` i then f (winding p !! n) else (winding p !! n)) $
                   [0..(length$winding p)-1]},i)
     )

modSelParts :: (Part -> Part) -> Game Editor ()
modSelParts f = modSel $ \(p,s) -> (f p,s)

modSelWindings' :: (Winding -> Winding) -> Game Editor ()
modSelWindings' f = modSel $ \(p,s) -> (p {winding=f (winding p)}, s)

mutateSel :: MU.Mutator -> Game Editor ()
mutateSel m = modSelWindings' (MU.mutate m)

-- move this more so into Winding.hs
stepIndexesWith :: [Part] -> (Step -> Bool) -> [(Part,[Int])]
stepIndexesWith ps f = for ps $ \p-> (p,findIndices f $ winding p)

selParts :: Editor -> [Part]
selParts ed = [ p | (p,_) <- selected ed ]

selSteps :: Editor -> [Step]
selSteps ed = concat $ map (\(p,i) -> map (winding p !!) i) $ selected ed

layerParts :: Editor -> [Part]
layerParts Editor {parts=ps,showingLayers=sl} = filter (\p -> sl `testBit` (layer p + 16)) ps

unlayerParts :: Editor -> [Part]
unlayerParts Editor {parts=ps,showingLayers=sl} = filter (\p -> not (sl `testBit` (layer p + 16))) ps

partsInLayer :: Int -> Editor -> [Part]
partsInLayer l ed = filter (\p -> (layer p) == l) $ (liftM2 (++) parts selParts) ed

allParts :: Editor -> [Part]
allParts = liftM2 (++) parts selParts

visibleParts :: Editor -> [Part]
visibleParts = liftM2 (++) selParts layerParts

writeModel :: String -> FilePath -> [Part] -> IO ()
writeModel n fn ps = do
    putStrLn $ "** Saving '"++fn++"'.."
    createDirectoryIfMissing True (takeDirectory fn)
    fn `writeFile` (show $ RModel {rParts=ps})

writeNVM :: String -> FilePath -> [Part] -> IO ()
writeNVM n fn ps = do
    createDirectoryIfMissing True $ takeDirectory fn
    fn `BC.writeFile` (compressWith (defaultCompressParams {compressLevel=BestCompression}) $ BC.pack (show RModel {rParts=ps}))
    putStrLn $ "\twrote '" ++ show fn ++ "'"

existingFileName :: Game Editor (Maybe String)
existingFileName =
    gets name >>= \n -> let fn = artDir++n++".model" in
    liftIO (doesFileExist fn) >>= \b ->
    return $ if b then Just fn else Nothing

reloadFile :: Game Editor ()
reloadFile = existingFileName >>=
    justM (\j-> do
        ps <- liftIO $ do putStrLn ("** Reading '"++j++"'..")
                          s <- readFile j
                          fmap rParts $ catch (readIO s >>= return) (\e-> error $ show e)

        clearSelection
        modify $ \ed->ed {parts=ps}
        pruneResources True)

pruneResources :: Bool -> Game Editor ()
pruneResources keepExisting =
    do liftIO $ readIORef resMap >>= unloadAllResources >> resMap `writeIORef` emptyResourceMap
       liftIO $ dirty -- resources are loaded upon rendering frame

saveFile :: Game Editor ()
saveFile =
    do ps <- gets allParts
       n  <- gets name
       liftIO $ writeModel n (artDir++n++".model") ps >> writeNVM n (contentDir++n++".nvm") ps

addContents :: (String,Performance) -> Game Editor ()
addContents (pn,c) =
    do modSelParts $ \p -> p {perf=c}
       modify $ \ed-> ed {withPerf=c, resourcePreview=if isResourceName pn then pn else resourcePreview ed}

removeContents :: (String,Performance) -> Game Editor ()
removeContents _ = return ()

{-
buildContents pn x =
    liftIO $ catch
               (liftM (Just . x) $ readIO pn)
               $ \e -> putStrLn (show e++": '"++pn++"'") >> return Nothing

addContents :: String -> Editor ()
addContents pn = do
    bm <- gets browsingMode

    c <- case bm of
        BRContents -> return Nothing
        BRProcedurals -> buildContents pn Proc
        BRSynthesizers -> buildContents pn Synth
        BRMutators -> buildContents pn Trans
        BRModels -> return$Just$Model pn
        BRImages -> return$Just$Textured pn
        BRSounds -> return$Just$Sound pn
        -- there is no way to select "Image"

    case c of
        Nothing -> return () --if bm == BRMutators then buildContents pn id >>=
      --          justM (\j->modSelParts (\p-> p {trans=j:trans p})) else return ()
        Just j -> do
            modSelParts (\p-> p {contents=contents p ++ [j]})
            modify (\ed->ed{withContents=[j], resourcePreview=if isResourceName pn then pn else resourcePreview ed})

removeContents :: String -> Editor ()
removeContents _ = return ()
{-
removeContents cn = do
    bm <- gets browsingMode

    case bm of
        BRMutators -> buildContents cn id >>= \m->
            case m of
            Nothing -> return ()
            Just j -> modSelParts (\p@(Part{trans=t})-> if null t then p else p {trans=delete j t})
        _ -> return ()
-}
-}

clearSelection :: Game Editor ()
clearSelection = modify $ \ed -> ed {parts=allParts ed, selected=[]}

selectAll :: (Part -> Bool) -> Game Editor ()
selectAll f = modify $ \ed -> let (sel,unsel) = f `partition` (visibleParts ed) in
                              ed {selected=map (flip (,) []) sel, parts=unsel++unlayerParts ed}

unselectAll :: (Part -> Bool) -> Game Editor ()
unselectAll f = modify $ \ed -> let (unsel,sel) = f `partition` (visibleParts ed) in
                                ed {selected=map (flip (,) []) sel, parts=unsel++unlayerParts ed}

selectAllFrom :: (Part -> Part -> Bool) -> Game Editor ()
selectAllFrom f = modify $ \ed->
    if (length$selParts ed) == 1 then  -- make sure
        let           s = head$selParts ed                  in
        let (sel,unsel) = (f s) `partition` (layerParts ed) in
        ed {selected=map (flip (,) []) $ sel++selParts ed, parts=unsel++unlayerParts ed}
        else ed

invertSelection :: Game Editor ()
invertSelection = modify (\ed->ed {selected= map (\p->(p,[])) $ parts ed, parts=selParts ed})

invertSelSteps :: Game Editor ()
invertSelSteps =
    modSel (\(p@(Part {winding=ss}),s) -> (p,filter (\n-> n `notElem` s) $ scanl (+) 0 $ replicate (length ss-1) 1))

selectWithPart :: Part -> Game Editor ()
selectWithPart sp@(Part {winding=w}) =
    gets gridSize >>= \sz-> selectAll (\p -> isPartInside sz p sp) >>
    modify (\ed->ed {selected = (selParts ed) `stepIndexesWith` (\(Step _ v _) -> (v,0) `isCircleInside` w) })

unselectWithPart :: Part -> Game Editor ()
unselectWithPart sp = gets gridSize >>= \sz-> unselectAll (\p -> isPartInside sz p sp)

selectVisible :: Game Editor ()
selectVisible =
    gets (camera.gameInfo) >>= \(_,_,z) ->
    -- scale by screen! =)
  --Editor {camera=(_,_,z)} <- getEditor
    let w = emptyPart { winding=[
        Step (0,0) (-512,-320) (0,0,0,0),
        Step (0,0) ( 512,-320) (0,0,0,0),
        Step (0,0) ( 512, 320) (0,0,0,0),
        Step (0,0) (-512, 320) (0,0,0,0)
        ]}
     in selectWithPart w

deleteSelected :: Game Editor ()
deleteSelected =
    gets selSteps >>= \ss->
    if not$null$ss
    then modSel (\(p,s) -> let p' = p {winding = filter (\s-> not$elem s$ss) $ winding p} in
                           if (not$null$(winding p')) && (length$winding p') > 1 then (p',[]) else (p,s) )
    else modify (\ed->ed {selected=[]})

toggleFill :: Game Editor ()
toggleFill =
    do modify $ \ed -> ed { showFill=not$showFill ed }
       modify $ \ed -> ed {showWire = if (not$showFill ed) && (not$showWire ed) then True else showWire ed }

toggleWire :: Game Editor ()
toggleWire =
    modify (\ed -> ed { showWire=not$showWire ed }) >>
    modify (\ed-> ed {showFill = if (not$showFill ed) && (not$showWire ed) then True else showFill ed })

{-
calcPartUV :: Scalar -> Part -> IO Part
calcPartUV sz p = liftM (\w-> p {winding = w}) $ mapM (calcStepUV sz p) (winding p)

calcStepUV :: Scalar -> Part -> Step -> IO Step
calcStepUV sz p (Step _ v@(x,y) c) = do
    r <- liftIO (resourceForContents $ contents p)
    let t = case r of
        Just (RImage {rWidth=w',rHeight=h'}) -> (x / (fromIntegral w'), y / (fromIntegral h'))
      --Nothing                              -> v
        Nothing                              -> (x / sz, y / sz)
    return (Step t v c)
  --return (Step (x/sz,y/sz) v c)

appendStep :: Scalar -> Part -> V2 -> IO Part
appendStep sz p@(Part {winding=w}) v@(x,y) =
    liftM (\s-> p{winding=MU.mutate (MU.AppendStep s) $ winding p}) (calcStepUV sz p $ Step v v (1,1,1,1))
-}

appendStep :: Scalar -> Part -> V2 -> Part
appendStep sz p@(Part {winding=w}) v =
    p {winding=MU.mutate (MU.AppendStep (Step (v/vecOf sz) v (1,1,1,1))) $ winding p}

cutSelected :: Game Editor ()
cutSelected = do
    start   <- gets cutStart
    end     <- liftGets (\(x,y,_) -> (x,y)) (camera.gameInfo)
    let cut = (fromJust start,end)
    sp      <- gets selParts

    let cps = concat $ for sp $ \p@(Part {winding=w}) -> let (left,right) = w `cutWithLine` cut  in
                                                         let s          j = [(p {winding=j},[])] in
                                                         maybe [] s left ++ maybe [] s right
    modify $ \ed -> ed {selected=cps}

