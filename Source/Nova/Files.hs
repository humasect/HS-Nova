module Nova.Files where

import System.Directory

import Common

-- TODO: Move Resource.hs bytestring, and gzip, all file ops, into here.

mkBackup :: FilePath -> IO ()
mkBackup fn = do
    let bk = fn ++ ".bk"
    b <- doesFileExist bk
    if b then removeFile bk else return ()
    copyFile fn bk

saveAs :: Show a => a -> FilePath -> IO ()
saveAs a fn = writeFile fn (show a) >> (putStrLn $ "\tsaved '"++show fn++"'.")

loadFrom :: Read a => a -> FilePath -> IO a
loadFrom a fn = do
    b <- doesFileExist fn
    m <- if b then readFile fn >>= return . maybeRead else return Nothing
    case m of
        Nothing -> putStrLn ("\tloading failed!: '"++show fn++"'") >> return a
        Just  j -> putStrLn ("\tloaded: '"         ++show fn++"'") >> return j

