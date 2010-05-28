module BuildContent where

--import System.Environment (getArgs)
--import System.Exit (exitWith,ExitCode(..))
import Control.Monad (when,liftM)

import System.Directory
import System.FilePath

import Codec.Image.PNG
import Data.Array.Storable
import Data.Array.IO
import Data.Array.IArray (bounds,elems)
import Data.Word
import Data.Int

import Codec.Wav as Wav
import Data.Audio

import qualified Data.ByteString.Lazy as BW
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Parser as P
import Codec.Compression.GZip

import Config (contentDir,artDir)
import Nova.Resource hiding (Resource(..))
import Nova.GL
import Nova.Procedural
import Nova.Mutator
--import Nova.Render
--import Nova.AL
import qualified Nova.Resource as New
import qualified Nova.Part as New
import qualified Nova.Winding as New
import Nova.HumaMath


pngToNovaImage :: String -> String -> IO ()
pngToNovaImage src dest = do
	png <- loadPNGFile src
	case png of
		Left l -> putStrLn ("Error: " ++ l)
		Right img -> writeImage (dimensions img) (if hasAlphaChannel img then 4 else 3) (imageData img)
	where
	writeImage :: (Width,Height) -> Int -> StorableArray (Int,Int) Word8 -> IO ()
	writeImage (w',h') d px = do
		let (w,h) = (fromIntegral w', fromIntegral h')
		let (w'''',h'''') = (fromIntegral w', fromIntegral h')
		let (ifmt,fmt) = if d>3 then (toConst RGBA8,toConst RGBA) else (toConst RGB8,toConst RGB)
		--writeFile dest (show NovaImage {width=w,height=h,depth=d,pixels=rows px (w,h) d})
		r <- rows px (w,h) d
		let px = compressWith BestCompression (BW.concat $ map BW.pack r)
		let out = BC.pack (show $
			NovaImage {nviWidth=w'''',nviHeight=h'''',nviInternalFormat=ifmt,nviFormat=fmt,nviPixels=px})

		BC.writeFile dest out
		putStrLn ("\t"++(show w)++"x"++(show h)++"x"++(show d))
		return ()
	--scanline img w d y = replicate (w*d) (readArray img 
	rows px (w,h) d = mapM (\n-> scanline px w d n) $ reverse [0..(h-1)]
	scanline px w d y = mapM (\n-> readArray px (y,n)) [0..((w*d)-1)]

-- just copy these three structures from Winding.hs and Resource.hs as the versions change.
data Step = Step V2 V2 V4      -- texcoord, vertex, color
	deriving (Show,Read,Eq)

data Winding = Winding {
	layer :: Int,
	normal :: V3,
	flags :: String,
	proc :: String, param :: String,
	isCurved :: Bool,
	steps :: [Step]
	}
	deriving (Show,Read)

data Resource = RModel { rWindings :: [Winding] }
	deriving (Show,Read)

newFromOld :: Winding -> New.Part
newFromOld n = New.Part
	{ New.layer = layer n
	, New.flags = flags n
	, New.travel = (0,1,1) -- normal n
	, New.contents = [New.Proc Default]
	, New.isCurved = isCurved n
	, New.isFan = False
	, New.winding = map (\(Step t v c) -> New.Step t v c) $ steps n
	}

modelToNovaModel :: String -> String -> IO ()
modelToNovaModel src dest = do
	-- see if it is old.
	s <- readFile src
	catch (readIO s >>= \r@(New.RModel {New.rParts=ps}) -> return () )
		(\e -> do
			putStrLn "\tmodel failed; trying old format, saving to .oldmodel and updating"
			let old = (dropExtension src) ++ ".oldmodel"
			renameFile src old
			s <- readFile old
			let RModel {rWindings=ows} = read s
			let nr = New.RModel {New.rParts = map newFromOld ows}
			writeFile src (show nr)
			putStrLn "ok.")

	BC.readFile src >>= BC.writeFile dest . compressWith BestCompression

wavToNovaSound :: String -> String -> IO ()
wavToNovaSound src dest = return ()
{-do
	f <- Wav.importFile src
	case f of
		Left l -> putStrLn ("Error: " ++ l)
		Right r -> do
			let a = (r :: Audio Word8)
			putStrLn ("\t" ++ (show $ sampleRate a) ++ "x"++(show $channelNumber a))
			--p <- Wav.parseWav a

			let (_,sz) = bounds (sampleData a)
			let fmt = fromIntegral $
				if (channelNumber a) == 1 then (toConst FormatMono16) else (toConst FormatStereo16)
			putStrLn ("\t"++(show $ (fromIntegral$sz) / (fromIntegral$(sampleRate a))) ++ " seconds.")
			--k <- samples (a sampleRate) (a sampleData)


			let smp = compressWith BestCompression (BW.pack $ elems (sampleData a))

			let out = BC.pack (show $
				NovaSound {nvsFormat=fmt,nvsFrequency=fromIntegral (sampleRate a),nvsSamples=smp})
			BC.writeFile dest out

			return ()
-}

main = do
	let collect dir = do
		d <- getDirectoryContents $ joinPath [artDir,dir]
		let res = filter (\n-> any (\t->takeExtension n==t) [".model",".png",".wav"]) d
		sub <- mapM (\n -> do
			b <- doesDirectoryExist $ joinPath [artDir,dir,n]
			if b then collect (joinPath [dir,n]) else return []
			) $ filter (\n -> n /= "." && n /= "..") d
		return $ (concat sub ++ (map (\n-> joinPath [dir,n]) res))

	x <- collect ""
	print x

	mapM_ (\n -> createDirectoryIfMissing False (joinPath [contentDir,(takeDirectory n)] )) x

	mapM_ (\n@(ext,f,dext)-> convert x n) [
		(".png", pngToNovaImage,".nvi"),
		(".wav", wavToNovaSound,".nvs"),
		(".model", modelToNovaModel,".nvm")]
	where

	convert files' (ext,f,dext) =
		let files = filter (\n-> takeExtension n == ext) files' in
		mapM_ (\s-> let
				t = dropExtension s
				(src,dest) = (artDir++t++ext, contentDir++t++dext)
				in checkConvert src f dest) files >>
		putStrLn ("++ "++(show $ length files)++" files.")
	checkConvert src f dest = do
		b <- doesFileExist dest
		go <- if b then do
			srct <- getModificationTime src
			destt <- getModificationTime dest
			if srct > destt then return True else return False
			else return True
		if go || (takeExtension src == ".model") then putStrLn (src++" -> "++dest) >> f src dest else return ()
