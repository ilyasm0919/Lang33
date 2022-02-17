{-# LANGUAGE LambdaCase, GADTs, KindSignatures #-}

module L33
    ( Monad, pure, lift
    , Eff, eff, Eff'(..), Handler, handle

    , cond, while

    , Num, Frac, Str, Unit, Void
    , Failable(..), Console
    , evalStateT, get, put

    , numToStr, fracToStr
    , numToFrac, fracToNum
    , parseNum, parseFrac
    
    , add, sub, mul
    , div, mod
    
    , addF, subF, mulF
    , divF
    
    , length
    , ord, chr
    , substring, setSubstring
    , print, getLine

    , absurd
    
    , runMain, P.IO) where

import Prelude (($), (.), flip, (<$>), fromInteger, (+), (-), (*), Fractional(..), floor, show, maybe, String, Monad(..), Applicative(..), Functor(..), Integer, Double, Bool, Read, (!!), otherwise, Ord (..), (&&), toInteger, take, drop, (++), splitAt, putStr)
import qualified Data.Char as P
import Text.Read (readMaybe)
import Data.Function (fix)
import Data.List (genericLength)
import Control.Monad ((<=<), (>=>), void, when)
import Data.Void (Void, absurd)
import Control.Monad.State (MonadTrans(..), evalStateT, get, put)
import qualified Prelude as P

type Num = Integer
type Frac = Double
type Str = String
type Unit = ()

data Eff' f m :: * -> * where
    Pure :: a -> Eff' f m a
    Eff :: f a -> (a -> Eff f m b) -> Eff' f m b

instance Functor m => Functor (Eff' f m) where
    fmap f (Pure x) = Pure $ f x
    fmap f (Eff x g) = Eff x $ fmap f . g

instance Applicative m => Applicative (Eff' f m) where
    pure = Pure
    Pure f <*> x = f <$> x
    Eff f g <*> x = Eff f $ EffT . fmap (<*> x) . runEffT . g

newtype Eff f m a = EffT { runEffT :: m (Eff' f m a) }

instance Functor m => Functor (Eff f m) where
    fmap f = EffT . fmap (fmap f) . runEffT

instance Applicative m => Applicative (Eff f m) where
    pure = EffT . pure . pure
    f <*> x = EffT $ (<*>) <$> runEffT f <*> runEffT x

instance Monad m => Monad (Eff f m) where
    mx >>= f = EffT $ runEffT mx >>= \case
        Pure x -> runEffT $ f x
        Eff x g -> pure . Eff x $ f <=< g

instance MonadTrans (Eff f) where
    lift = EffT . fmap pure

data Failable a where
    Fail :: Str -> Failable Void

data Console a where
    IO :: P.IO a -> Console a

runMain :: Eff Console P.IO a -> P.IO ()
runMain = runEffT >=> \case
    Pure _ -> pure ()
    Eff (IO x) cont -> x >>= runMain . cont

eff :: Applicative m => f a -> Eff f m a
eff = EffT . pure . flip Eff pure

cond :: Num -> Bool
cond = (> 0)

while :: Monad m => m Num -> m a -> m Unit
while c x = c >>= flip when (x >> while c x) . cond

type Handler f a m b = Eff f m a -> m b
type Handler' f a m b = Eff' f m a -> m b

handle :: Monad m => (Handler f a m b -> Handler' f a m b) -> Handler f a m b
handle f = fix $ \h x -> runEffT x >>= f h

unary :: Applicative m => (a -> b) -> a -> m b
unary f = pure . f

binary :: Applicative m => (a -> b -> c) -> a -> b -> m c
binary f = fmap pure . f

fail :: Applicative m => Str -> Eff Failable m a
fail = fmap absurd . eff . Fail


numToStr :: Applicative m => Num -> m Str
numToStr = unary show
fracToStr :: Applicative m => Frac -> m Str
fracToStr = unary show
numToFrac :: Applicative m => Num -> m Frac
numToFrac = unary fromInteger
fracToNum :: Applicative m => Frac -> m Num
fracToNum = unary floor
parse :: (Monad m, Read a) => Str -> Eff Failable m a
parse = maybe (fail "Parsing failed") pure <=< unary readMaybe
parseNum :: Monad m => Str -> Eff Failable m Num
parseNum = parse
parseFrac :: Monad m => Str -> Eff Failable m Frac
parseFrac = parse
add :: Applicative m => Num -> Num -> m Num
add = binary (+)
sub :: Applicative m => Num -> Num -> m Num
sub = binary (-)
mul :: Applicative m => Num -> Num -> m Num
mul = binary (*)
div :: Applicative m => Num -> Num -> Eff Failable m Num
div n 0 = fail "Divide by zero"
div n m = pure $ n `P.div` m
mod :: Applicative m => Num -> Num -> Eff Failable m Num
mod n 0 = fail "Divide by zero"
mod n m = pure $ n `P.mod` m
addF :: Applicative m => Frac -> Frac -> m Frac
addF = binary (+)
subF :: Applicative m => Frac -> Frac -> m Frac
subF = binary (-)
mulF :: Applicative m => Frac -> Frac -> m Frac
mulF = binary (*)
divF :: Applicative m => Frac -> Frac -> Eff Failable m Frac
divF n 0 = fail "Divide by zero"
divF n m = pure $ n / m
length :: Applicative m => Str -> m Num
length = unary genericLength
ord :: Applicative m => Str -> Num -> Eff Failable m Num
ord s x
    | x >= 0 && x < genericLength s = pure . toInteger . P.ord $ s !! fromInteger x
    | otherwise = fail "Index out of range"
chr :: Applicative m => Num -> m Str
chr = unary $ pure . P.chr . P.fromInteger
substring :: Applicative m => Str -> Num -> Num ->
    Eff Failable m Str
substring s x y
    | x >= 0 && y >= 0 && x + y <= genericLength s =
        pure $ take (fromInteger y) $ drop (fromInteger x) s
    | otherwise = fail "Index out of range"
setSubstring :: Applicative m => Str -> Num -> Num -> Str ->
    Eff Failable m Str
setSubstring s x y s'
    | x >= 0 && y >= 0 && x + y <= genericLength s =
        let (h, t) = splitAt (fromInteger x) s in
        pure $ h ++ s' ++ drop (fromInteger y) t
    | otherwise = fail "Index out of range"
print :: Applicative m => String -> Eff Console m ()
print = eff . IO . putStr
getLine :: Monad m => Eff Console m String
getLine = eff $ IO P.getLine
