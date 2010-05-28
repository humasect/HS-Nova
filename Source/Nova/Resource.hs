module Nova.Resource (
    emptyResourceMap,resMap,withResource,
    findResource,
    Resource(..),RImage,RModel,RSound,RScript,RPerformance,
    ResKey,ResourceMap,

    loadResources,loadResource,

--    findAllResourceDirs,findAllResNamesInSub,
    --allImages,allModels,allResourceNames,
    unloadAllResources,
    --reloadContentsMaps,
    
    isResourceName,
  --safeFindImage,safeFindModel,safeFindSound,
    safeReadScript,
    -- for BuildContents
    NovaImage(..),NovaSound(..)
) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BW
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Codec.Compression.GZip
import qualified Data.Map as Map
import Data.Map               (Map)
import Data.Word
import Data.IORef
import System.IO.Unsafe       (unsafePerformIO)
import Data.List              (partition)
import System.Directory
import System.FilePath
import Data.Maybe
import Foreign.Ptr
import Foreign.Marshal.Utils  (with)

import Common
import Config                 (artDir,contentDir)
import qualified Nova.AL as AL
import Nova.GL
import Nova.HumaMath
import Nova.Part

data NovaImage = NovaImage {
    nviWidth, nviHeight :: GLsizei,
      nviInternalFormat :: GLenum,
              nviFormat :: GLenum,
              nviPixels :: BC.ByteString
    }
  deriving (Show,Read)

data NovaSound = NovaSound {
       nvsFormat :: AL.ALsizei,
    nvsFrequency :: AL.ALsizei,
      nvsSamples :: BC.ByteString
    }
  deriving (Show,Read)

type ResKey = String

type ResourceMap = Map ResKey Resource

type RModel  = Resource
type RImage  = Resource
type RSound  = Resource
type RScript = Resource
type RPerformance = Resource

--data (Read a,Show a) => ScriptCode a = Script a | DictionaryScript [(String,a)]

data Resource =
      RImage  { rWidth, rHeight :: GLsizei, rTexID :: GLuint }
    | RSound    { rDuration :: GLfloat, rBuffer :: AL.ALuint }
    | RModel                              { rParts :: [Part] }
    | RScript     String

    | RPerfList   [(String,Performance)]
  deriving (Show,Read,Eq)

emptyResourceMap :: ResourceMap
emptyResourceMap = Map.fromList [
    ("empty.nvi",RImage 16 16 0),
    ("empty.nvs",RSound     0 0),
    ("empty.nvm",RModel      []),
    ("empty.nhs",RScript     ""),
    ("empty.npl",RPerfList   [])
    ]

{-# NOINLINE resMap #-}
resMap :: IORef ResourceMap
resMap = unsafePerformIO $ newIORef emptyResourceMap

withResource :: ResKey -> (Resource -> IO ()) -> IO ()
withResource s f = findResource resMap s >>= \r-> f r

loadImage :: ResKey -> IO Resource
loadImage fn = do
    putStrLn $ "loadImage '"++fn++"'" 
    i@(NovaImage {nviWidth=w,nviHeight=h}) <- loadNovaImage $ contentDir ++ fn

    enable & Texture2D
    n <- glGen genTextures
    (bindTexture & Texture2D) n
    unsafeUseAsCStringLen (BW.concat $ BC.toChunks $ nviPixels i)
        (\(ptr,len)-> (texImage2D & Texture2D) 0 (nviInternalFormat i) w h 0 (nviFormat i) (toConst UnsignedByte) ptr)
            --build2DMipmaps (toConst Texture2D) (nviInternalFormat i) w h   (nviFormat i) (toConst UnsignedByte) ptr)

    (texParameteri & Texture2D) (toConst TextureMinFilter) (toConst Nearest)
    (texParameteri & Texture2D) (toConst TextureMagFilter) (toConst Nearest)

  --generateMipmapsEXT & Texture2D
    (bindTexture & Texture2D) 0
    disable & Texture2D

    return $ RImage w h n
    where
    loadNovaImage f = liftM (\i-> i {nviPixels = decompress (nviPixels i)}) $ BC.readFile f >>= readIO . BC.unpack

loadSound :: ResKey -> IO Resource
loadSound fn = do
    putStrLn $ "loadSound '"++fn++"'" 
    s <- loadNovaSound $ contentDir ++ fn

    n <- glGen AL.genBuffers
    unsafeUseAsCStringLen (BW.concat $ BC.toChunks $ nvsSamples s) (\(ptr,len)->
        AL.bufferData n (nvsFormat s) ptr (fromIntegral len) (nvsFrequency s))

    return RSound {rDuration=0, rBuffer=n}
    where
    loadNovaSound f = do
        x <- BC.readFile f
        let s = read $ BC.unpack x
        return s {nvsSamples = decompress $ nvsSamples s}

loadModel :: ResKey -> IO Resource
loadModel fn = do
    putStrLn $ "loadModel '"++fn++"'" 
    liftM (\x-> read $ BC.unpack (decompress x)) $ BC.readFile (contentDir ++ fn)

loadScript :: ResKey -> IO Resource
loadScript fn = do
    putStrLn $ "loadScript '"++fn++"'"
    let n = artDir++fn
    liftM RScript $ readFile n

loadPerfList :: ResKey -> IO Resource
loadPerfList fn = do
    putStrLn $ "loadPerfList '"++fn++"'"
    let n = artDir++fn

    let x = (takeExtension.dropExtension) n

    r <- liftM RPerfList $ if (not.null) x then contentsFiles x else (safeRead n =<< readFile n) :: IO [(String,Performance)]
    return r

    where
    contentsFiles ext = do
        dirs <- findAllResourceDirs ""
        ds   <- mapM findAllResNamesInSub dirs
        let xs = (filter (\n -> takeExtension n == ext) . concat) ds
        return $ map (\n -> (n,Ethereal [])) xs

loadResource :: ResourceMap -> String -> IO ResourceMap
loadResource rm f =
    if (Map.member f rm) then return rm
    else let m = case takeExtension f of
                   ".nvi" -> Just $ loadImage  f
                   ".nvm" -> Just $ loadModel  f
                   ".nvs" -> Just $ loadSound  f
                   ".nhs" -> Just $ loadScript f
                   ".npl" -> Just $ loadPerfList f
                   _      -> Nothing
        in maybe (return rm) (\j-> j >>= \r-> return $ Map.insert f r rm) m

unloadAllResources :: ResourceMap -> IO ()
unloadAllResources rm = do
    putStrLn "unloading resources.."

    mapM_ (\(k,a) -> unloadResource a) $ Map.toList rm
    -- remove entries
    -- using a timeout ?
    where
    unloadResource r = case r of
        RImage  {rTexID=texID} -> with texID $ deleteTextures 1
        RSound {rBuffer=bufID} -> with bufID $ AL.deleteBuffers 1
        _ -> return ()

loadResources :: ResourceMap -> [ResKey] -> IO ResourceMap
loadResources rm resNames = do
    -- does not remove duplicates.
    foldl (\m n -> m >>= \x' -> loadResource x' n) (return rm) resNames

{-
safeFindImage :: ResourceMap -> String -> RImage
safeFindImage rm k = Map.findWithDefault emptyImage k rm

safeFindSound :: ResourceMap -> String -> RSound
safeFindSound rm k = Map.findWithDefault emptySound k rm

safeFindModel :: ResourceMap -> String -> RModel
safeFindModel rm k = Map.findWithDefault emptyModel k rm
-}

findResource :: IORef ResourceMap -> ResKey -> IO Resource
findResource irm k =
    readIORef irm >>= \rm ->
    case (Map.lookup k rm) of
        Nothing -> loadResource rm k >>= \rm -> writeIORef irm rm >> case Map.lookup k rm of
            Just  j -> return j
            Nothing -> return $ RScript "NoMethodError (\"Resource Not Loaded.\")" -- emptyImage -- hmm...
        Just  j -> return j

safeReadScript :: Read a => IORef ResourceMap -> ResKey -> IO a
safeReadScript irm n =
    do RScript s <- irm `findResource` n
       safeRead n s
     --catch (readIO s) (\e-> putStrLn ("safeReadScript: could not read '"++n++"':"++show e) >> return [])

------------------------------------------------------------

{-
allResourceNames :: ResourceMap -> [ResKey]
allResourceNames rm = fst.unzip $ Map.toList rm

allImages :: ResourceMap -> [Resource]
allImages rm = [a | (k,a@RImage {}) <- Map.toList rm]

allModels :: ResourceMap -> [Resource]
allModels rm = [a | (k,a@RModel {}) <- Map.toList rm]
-}

{-
reloadContentsListings :: IORef ResourceMap -> IO ()
reloadContentsListings _rm =
    do rm <- readIORef
       rm <- loadResource rm 
-}

reloadContentsMaps :: IORef ResourceMap -> IO ()
reloadContentsMaps _rm = do
    dirs <- findAllResourceDirs ""
    ds   <- mapM findAllResNamesInSub dirs
    --let names = filter (\n -> (takeExtension n == r)
    print ds

isResourceName :: ResKey -> Bool
isResourceName n = let t x = takeExtension n == x in any t [".nvm",".nvi",".nvs",".nhs",".npl"]

findAllResNamesInSub :: FilePath -> IO [FilePath]
findAllResNamesInSub dir =
    liftM (filter isResourceName) (getDirectoryContents $ joinPath [contentDir,dir]) >>=
    return . map (\n-> if dir /= "." then joinPath [dir,n] else n)

findAllResourceDirs :: FilePath -> IO [FilePath]
findAllResourceDirs dir = do
    ds  <- liftM (filter (\n -> n /= "." && n /= "..")) $ getDirectoryContents (joinPath [contentDir,dir])
    sub <- mapM (\n -> do b <- doesDirectoryExist $ joinPath [contentDir,dir,n]
                          if b then findAllResourceDirs (joinPath [dir,n]) else return []) ds
    return $ (if dir=="" then "." else dir) : concat sub

