{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror -cpp #-}

module Text.Happy.ParserMonad
  (P, parseError, mkParser, Pos(..), noPos, HasPos(..), AddPos(..), at)
  where

import Control.Monad.Error(ErrorT, runErrorT, MonadError(..))
import Control.Monad.Identity(Identity, runIdentity)
--import System.FilePath(FilePath)
import Data.Data(Data, Typeable)

import Text.PrettyPrint.HughesPJ(text, (<>), colon, int)
import Text.PrettyPrint.Pp(Pp(..))

newtype P a = P{ unP :: ErrorT String Identity a }
#ifndef __HADDOCK__
  deriving (Monad, MonadError String, Functor)
#endif

_P_not_used :: a
_P_not_used = undefined P

parseError :: Show t => [t] -> P a
parseError []     = throwError "Parse error at EOF"
parseError (t:_) = throwError $ "Parse error at " ++ show t

mkParser :: (String -> P a) -> String -> Either String a
mkParser p = runIdentity . runErrorT . unP . p

data Pos = Pos FilePath !Int !Int !Int
  deriving (Read, Show, Eq, Ord, Data, Typeable)

noPos :: Pos
noPos = Pos "" 0 0 0

------------------------------------------------------------

instance Pp Pos where
  pp (Pos fname _ l c) = text fname <> colon <> int l <> colon <> int c

------------------------------------------------------------

class HasPos t where
  getPos :: t -> Maybe Pos

class HasPos t => AddPos t where
  addPos  :: t -> Pos -> t
  dropPos :: t -> t

instance HasPos Pos where
  getPos = Just

instance AddPos Pos where
  addPos _ p = p
  dropPos  p = p

at :: (HasPos l, AddPos t) => l -> t -> t
at l e = maybe e (addPos e) (getPos l)
