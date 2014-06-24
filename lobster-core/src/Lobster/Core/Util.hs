--
-- Util.hs --- General purpose utilities.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core.Util where

import Control.Monad

-- | "when" with a monadic boolean condition.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= (\x -> when x f)

-- | "unless" with a monadic boolean condition.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b f = b >>= (\x -> unless x f)


