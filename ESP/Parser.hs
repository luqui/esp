module ESP.Parser where

import Control.Applicative (Applicative(..), liftA2)
import Control.Monad (ap, (>=>), MonadPlus(..), guard)
import qualified Data.Char as Char

data Parser i a
    = Get (i -> Parser i a)
    | Result a (Parser i a)
    | Fail

instance Functor (Parser i) where
    fmap f (Get c) = Get ((fmap.fmap) f c)
    fmap f (Result x p) = Result (f x) (fmap f p)
    fmap f Fail = Fail

instance Monad (Parser i) where
    return x = Result x Fail
    Get c      >>= f = Get (c >=> f)
    Result x p >>= f = f x `mplus` (p >>= f)
    Fail       >>= _ = Fail

instance MonadPlus (Parser i) where
    mzero = Fail
    Result x p `mplus` q          = Result x (p `mplus` q)
    p          `mplus` Result x q = Result x (p `mplus` q)
    Fail       `mplus` p          = p
    p          `mplus` Fail       = p    
    Get c      `mplus` Get c'     = Get (liftA2 mplus c c')

instance Applicative (Parser i) where
    pure = return
    (<*>) = ap

input :: Parser i i
input = Get return

satisfies :: (MonadPlus m) => (a -> Bool) -> m a -> m a
satisfies p m = do
	x <- m
	guard (p x)
	return x

many :: Parser i a -> Parser i [a]
many p = return [] `mplus` liftA2 (:) p (many p)

identifier :: Parser Char String
identifier = liftA2 (:) (satisfies Char.isAlpha input) (many (satisfies Char.isAlphaNum input))

whitespace :: Parser Char ()
whitespace = many (satisfies Char.isSpace input) >> return ()

symbol :: String -> Parser Char String
symbol s = go s
    where
    go [] = return s
    go (x:xs) = satisfies (== x) input >> go xs
