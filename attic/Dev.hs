module Dev where

import System.Cmd (system)
import System.Time
import System.Locale
import System.Environment (getEnv)
import System.Directory

novaRoot = "/Users/humasect/Novapilot/" :: FilePath

command cmd = putStrLn cmd >> system cmd >> return ()

makeBackups = do
	s <- timestamp
	let fileName = "Novapilot-" ++ s ++ ".tar"

	command $ "tar -cvf ../" ++ fileName ++ " ../Novapilot"  -- use pwd here
	command $ "bzip2 -9 ../" ++ fileName

	let bz2file = fileName ++ ".bz2"
	let tarfile = novaRoot ++ "../" ++ bz2file
	mapM_ (\d -> copyFile tarfile (d ++ bz2file)) ["/", "/Users/humasect/Documents/"]
	removeFile tarfile
	return ()
	where timestamp = getClockTime >>= toCalendarTime >>= \y ->
		return $ formatCalendarTime defaultTimeLocale "%Y.%m.%d" y

-- |Install the Blender export script into the .app 'bp'
blenderInstall = blenderInstall' "~/blender3/blender.app"
blenderInstall' :: FilePath -> IO ()
blenderInstall' bp = do
	let dest = bp ++ "/Contents/MacOS/"
	command $ "cp ./Code/novapilot_export.py " ++ dest ++ ".blender/scripts"
	putStrLn ("Be sure add this line to " ++ dest ++ ".blender/Bpymenus :")
	putStrLn "'Novapilot Model (.hs)' 242 novapilot_export.py 0 'Export model to Novapilot Format'"

make =
	--command $ "ghc -iCode -iContent --make Main"
	command $ "./mk.sh"

-- |darcs record /comment/
darcsRecord :: String -> IO ()
darcsRecord a = command $ "darcs record --patch-name=\"" ++ a ++ "\" --all --skip-long-comment"

-- |darcs add -r /filename/
darcsAdd :: String -> IO ()
darcsAdd a = command $ "darcs add -r " ++ a

-- |darcs remove /filename/
darcsRemove :: String -> IO ()
darcsRemove a = command $ "darcs remove " ++ a

-- |darcs mv /srcfile/ /destfile/
darcsMove :: String -> String -> IO ()
darcsMove a b = command $ "darcs mv " ++ a ++ " " ++ b

-- |darcs whatsnew -s -l
darcsWhatsNew :: IO ()
darcsWhatsNew = command $ "darcs whatsnew -s -l"

-- |make docs
makeDocs :: IO ()
makeDocs = command $ "make docs"

---------------------------
