module Common (
    leftsOf,rightsOf,
    whenLeft,whenRight,
    isLeft,isRight,

    whenM,unlessM,whenM2,
    justM,nothingM,
    stringToMaybe,maybeToString,

    rotatel,rotater,
    maybeRead,safeRead,

    liftGets,liftGets2,
    for,
    refLoop,

    deleteIndex,dbl,

    -- from Control.Monad
    when,forM_,forM,liftM,liftM2
) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe (catMaybes)
import Data.Char  (isSpace)
import Data.IORef

--n `on1` f = \x -> f a `n` f a

leftsOf   :: [Either a b] -> [a]
leftsOf xs = [ s | Left s <- xs ]

rightsOf   :: [Either a b] -> [b]
rightsOf xs = [ s | Right s <- xs ]

whenLeft           :: Monad m => Either a b -> m () -> m ()
whenLeft (Left a) f = f
whenLeft _ _        = return ()

whenRight            :: Monad m => Either a b -> m () -> m ()
whenRight (Right a) f = f
whenRight _ _         = return ()

isLeft         :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight          :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

whenM    :: Monad m => m Bool -> m () -> m ()
whenM p s = p >>= \b-> if b then s else return ()

unlessM    :: Monad m => m Bool -> m () -> m ()
unlessM p s = p >>= \b-> if not b then s else return ()

--whenM' :: Monad m => m a -> (a -> Bool) -> m () -> m ()
--whenM' p t s = p >>= \b-> if t b then s else return ()

whenM2      :: Monad m => m Bool -> m a -> m a -> m a
whenM2 p s v = p >>= \b-> if b then s else v

justM           :: (Monad m) => (t -> m ()) -> Maybe t -> m ()
justM _ Nothing  = return ()
justM f (Just x) = f x

nothingM           :: (Monad m) => m () -> Maybe t -> m ()
nothingM f Nothing  = f
nothingM _ (Just _) = return ()

maybeToString :: Maybe String -> String
maybeToString  Nothing = []
maybeToString (Just j) = j

stringToMaybe :: String -> Maybe String
stringToMaybe [] = Nothing
stringToMaybe xs = Just xs

rotatel   :: [a] -> [a]
rotatel ns = tail ns ++ [head ns]
--transpose ns = foldr (:) [head ns] (tail ns)

rotater :: [a] -> [a]
rotater  = rotatel.reverse

maybeRead  :: Read a => String -> Maybe a
maybeRead s = case reads s of { [(x, rest)] | all isSpace rest -> Just x; _ -> Nothing }
---[(x, "")] -> Just x;

safeRead :: Read a => String -> String -> IO a
safeRead n s =
    catch (readIO s) (\e-> error ("safeRead: could not read '"++n++"':"++show e))

liftGets    :: (MonadState s m) => (a1 -> r) -> (s -> a1) -> m r
{-# INLINE liftGets #-}
liftGets k f = liftM k (gets f)

liftGets2      :: (MonadState s m) => (a1 -> a2 -> r) -> (s -> a1) -> (s -> a2) -> m r
{-# INLINE liftGets2 #-}
liftGets2 k f g = liftM2 k (gets f) (gets g)

for :: [a] -> (a -> b) -> [b]
{-# INLINE for #-}
for  = flip map

deleteIndex     :: Int -> [a] -> [a]
deleteIndex i as = catMaybes $ map (\(a,n) -> if n == i then Nothing else Just a) $ zip as [0..(length as)-1]

dbl  :: a -> (a,a)
dbl a = (a,a)

refLoop :: (Num n, MonadIO m) => (n -> b -> m a) -> (n,n) -> [b] -> m [a]
refLoop f (st,inc) as = do
    rn  <- liftIO $ newIORef st
    mas <- mapM (\a -> do n <- liftIO $ readIORef rn
                          c <- f n a
                          liftIO $ writeIORef rn (n+inc)
                          return c) as
    return mas

