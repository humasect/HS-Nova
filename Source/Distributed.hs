module Distributed (
	client,server,
	--say,hear,
	Message(..)
) where

-- TODO: use network-bytestring but its low level sockets. will need to convert handles from Network

import Network
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans
import System.IO
import System.IO.Unsafe
import Data.IORef
import Data.Map as Map

data Message = Hello | Goodbye | Null
	deriving (Show,Read)

{-# NOINLINE incoming #-}
{-# NOINLINE outgoing #-}
incoming :: Chan Message
incoming = unsafePerformIO $ newChan

outgoing :: Chan Message
outgoing = unsafePerformIO $ newChan

server :: IO ()
server = do
	s <- listenOn (PortNumber 12345)
	csR <- newIORef Map.empty
	forkIO $ acceptLoop s csR
	forkIO $ responseLoop csR
	loop csR
	sClose s
	where
	acceptLoop s csR = do
		(h,hn,pn) <- accept s
		putStrLn $ (show hn) ++ " connected."

		cs <- readIORef csR
		writeIORef csR (Map.insert hn h cs)
		acceptLoop s csR

	responseLoop csR = do
		-- reads from list
		cs <- readIORef csR
		if Map.size cs < 1 then threadDelay 25000 else
			mapM_ (\(hn,h) -> collectIncoming hn h >>= \b->
				when (not$b) $ writeIORef csR (Map.delete hn cs)
			) $ Map.toList cs
		responseLoop csR

	loop csR = do
		cs <- readIORef csR
		mapM_ (\(hn,h) -> deliverOutgoing hn h) $ Map.toList cs
		loop csR

collectIncoming :: HostName -> Handle -> IO Bool
collectIncoming hn h = catch (do
	b <- hWaitForInput h 250
	when b $ do
		s <- hGetLine h
		putStrLn $ "from "++show hn++": "++s
		writeChan incoming (read s)
	return True
	) (\e-> putStrLn (hn ++ ": "++show e) >> return False)

deliverOutgoing :: HostName -> Handle -> IO ()
deliverOutgoing hn h = do
	b <- isEmptyChan outgoing
	if b then threadDelay 25000 else do
		m <- readChan outgoing
		hPutStrLn h (show m)
		hFlush h

client :: HostName -> IO ()
client hn = do
	h <- connectTo hn (PortNumber 12345)
	forkIO $ responseLoop hn h
	loop hn h
	hClose h
	where
	responseLoop hn h = collectIncoming hn h >>= \b-> if b then responseLoop hn h else return ()

	loop hn h = deliverOutgoing hn h >> loop hn h

{-
say :: MonadIO m => Message -> m ()
say m = liftIO $ writeChan outgoing m

hear :: MonadIO m => (Message -> m ()) -> IO ()
hear f = do
	b <- liftIO $ isEmptyChan incoming
	if b then return () else do
		liftIO $ readChan incoming >>= f
		hear f
-}