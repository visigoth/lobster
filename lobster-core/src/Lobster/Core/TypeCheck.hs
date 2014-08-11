{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- TypeCheck.hs --- Lobster connection type checker.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.Core.TypeCheck (
  tc
  ) where

import Control.Applicative ((<$>))
import Control.Error
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid ((<>))
import Data.Text (Text)

import Lobster.Core.AST
import Lobster.Core.Error
import Lobster.Core.Eval

import qualified Data.Map as M
import qualified Data.Set as S

-- | Mapping from ports to connections that reference them.
type PortConnMap = M.Map PortId (S.Set ConnectionId)

-- | The state of the type checker---a Lobster module and the set of
-- connections that need to be type checked.  If type information
-- is inferred for a connection, that may necessitate re-checking
-- other connections to the ports of the connection that was just
-- checked.
--
-- We also build a reverse mapping of ports to connections that
-- mention that port, so we can mark connections for re-checking
-- when port types are inferred.
data TCState l = TCState
  { _tcModule       :: Module l
  , _tcPortConns    :: PortConnMap
  , _tcConnections  :: S.Set ConnectionId
  } deriving Show

makeLenses ''TCState

type TC l a = StateT (TCState l) (Either (Error l)) a

-- | Add connection IDs to the port-connection map for the left
-- and right ports of a connection.
addPortConn :: PortConnMap -> ConnectionId -> Connection l -> PortConnMap
addPortConn pcMap connId conn = go lPort (go rPort pcMap)
  where
    lPort = conn ^. connectionLeft
    rPort = conn ^. connectionRight
    go pid m = M.insert pid (S.union (M.findWithDefault S.empty pid m)
                                     (S.singleton connId)) m

-- | Build the port-connection map given an initial type checker state.
addPortConns :: TCState l -> TCState l
addPortConns tcSt = tcPortConns .~ pcMap $ tcSt
  where
    conns = tcSt ^. tcModule . moduleConnections
    pcMap = M.foldlWithKey' addPortConn M.empty conns

-- | Return text for a position.
ppPos :: Position -> Text
ppPos PosUnknown = "unknown"
ppPos PosSubject = "subject"
ppPos PosObject  = "object"

-- | Return the position of the left port of a connection.
leftPos :: Connection l -> TC l Position
leftPos conn = do
  m <- use tcModule
  let level = conn ^. connectionLevel
  let port  = m ^. idPort (conn ^. connectionLeft)
  let pos   = port ^. portPosition

  case level of
    ConnLevelParent   -> return $! revPosition pos
    ConnLevelChild    -> return $! pos
    ConnLevelInternal -> return $! revPosition pos
    ConnLevelPeer     -> return $! pos

-- | Return the position of the right port of a connection.
rightPos :: Connection l -> TC l Position
rightPos conn = do
  m <- use tcModule
  let level = conn ^. connectionLevel
  let port  = m ^. idPort (conn ^. connectionRight)
  let pos   = port ^. portPosition

  case level of
    ConnLevelParent   -> return $! pos
    ConnLevelChild    -> return $! revPosition pos
    ConnLevelInternal -> return $! revPosition pos
    ConnLevelPeer     -> return $! pos

-- | Set the position of a port and mark all connections that
-- reference that port as needing to be rechecked.
tcInferPos :: PortId -> Position -> TC l ()
tcInferPos pid pos = do
  tcModule . modulePorts . ix pid . portPosition .= pos
  conns <- use $ tcPortConns . ix pid
  tcConnections %= S.union conns

-- | Type check a single connection.
--
-- TODO: We could simplify all the cases on 'level' here by
-- having some accessors on 'Connection' to return the positions
-- of the left and right ports.
tcConn :: ConnectionId -> TC l ()
tcConn connId = do
  m <- use tcModule
  let conn = m ^. idConnection connId
  let level = conn ^. connectionLevel

  -- get the left and right ports
  let lPid  = conn ^. connectionLeft
  let lPort = m ^. idPort lPid
  let lPos1 = lPort ^. portPosition

  let rPid  = conn ^. connectionRight
  let rPort = m ^. idPort rPid
  let rPos1 = rPort ^. portPosition

  -- flip the appropriate port position if connection to the
  -- internal side of a port.
  let (lPos2, rPos2) =
        case level of
          ConnLevelParent   -> (revPosition lPos1, rPos1)
          ConnLevelChild    -> (lPos1, revPosition rPos1)
          ConnLevelPeer     -> (lPos1, rPos1)
          ConnLevelInternal -> (revPosition lPos1, revPosition rPos1)

  case (lPos2, rPos2) of
    -- if both port postions are unknown, don't do anything
    (PosUnknown, PosUnknown) -> return ()

    -- if one port position is unknown and the other is not, infer
    -- the position and re-add that ports connections to the TC set.
    --
    -- the inferred position is opposite the known position unless
    -- the connection is to the inside of the unknown port.
    (_, PosUnknown) -> tcInferPos rPid $ case level of
                         ConnLevelChild    -> lPos2
                         ConnLevelInternal -> lPos2
                         _                 -> revPosition lPos2
    (PosUnknown, _) -> tcInferPos lPid $ case level of
                         ConnLevelParent   -> rPos2
                         ConnLevelInternal -> rPos2
                         _                 -> revPosition rPos2

    -- if both positions are known, require them to agree
    (PosSubject, PosObject)  -> return ()
    (PosObject,  PosSubject) -> return ()

    _ -> lift $ do
      let lPath = lPort ^. portPath
      let rPath = rPort ^. portPath
      let lExtra = case level of
                     ConnLevelParent   -> " (internal)"
                     ConnLevelInternal -> " (internal)"
                     _                 -> ""
      let rExtra = case level of
                     ConnLevelChild    -> " (internal)"
                     ConnLevelInternal -> " (internal)"
                     _                 -> ""
      throwE $ BadPosition (label conn) (lPath <> lExtra)
                           (rPath <> rExtra)
                           (ppPos lPos2 <> " to " <> ppPos rPos2)

-- | Type check all connections in the current module.
tcConns :: TC l ()
tcConns = do
  s <- use tcConnections
  case S.minView s of
    Just (connId, s') -> do
      tcConnections .= s'
      tcConn connId
      tcConns
    Nothing -> return ()

-- | Normalize a connection so the subject port (if any) is
-- on the left hand side.
tcNormalize1 :: ConnectionId -> TC l ()
tcNormalize1 connId = do
  m        <- use tcModule
  let conn  = m ^. idConnection connId
  lPos     <- leftPos  conn
  rPos     <- rightPos conn

  case (lPos, rPos) of
    (PosObject, PosSubject) -> do
      tcModule . moduleConnections . ix connId %= revConn
    _ -> return ()

-- | Normalize all connections to put the subject port on the
-- left hand side.
tcNormalize :: TC l ()
tcNormalize = do
  m <- use tcModule
  mapM_ tcNormalize1 (m ^. moduleConnections . to M.keys)

-- | Type check a Lobster module.
tc :: Module l -> Either (Error l) (Module l)
tc m = view tcModule <$> execStateT go (addPortConns st)
  where
    go = tcConns >> tcNormalize
    st = TCState m M.empty (M.keysSet $ m ^. moduleConnections)

