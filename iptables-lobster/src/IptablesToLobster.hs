{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module IptablesToLobster (toLobster, parseIptables, Error(..)) where

import Prelude hiding (drop)

import Control.Applicative
import Control.Error
import Control.Monad.RWS

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

import Iptables
import Iptables.Parser
import Iptables.Print
import Iptables.Types

import qualified SCD.Lobster.Gen.CoreSyn as L

-- | Translate 'Iptables' into Lobster. If we encounter an error along
-- the way, discard the translation and return the error.
--
-- TODO: Return multiple errors if we can find them
toLobster :: Iptables -> Either Error [L.Decl]
toLobster ipts =
    case runM ipts table0 chain0 accept0 translateAll of
      (Left e, _)      -> Left e
      (Right _, final) ->
        return $ preamble ++ [mkHost "Host" (stateToLobster final)] ++ [host]
  where host = L.newDomain "host" "Host" []
        -- separate these out so we have the correct initial values;
        -- undefined is okay for now on these because it gets reset
        -- immediately by translateChain
        table0 = undefined
        chain0 = undefined
        accept0 = undefined
        translateAll = do
          e0 <- translateChain "raw" "PREROUTING"
          addEdge (incoming, e0)
          e1 <- translateChain "mangle" "INPUT"
          addEdge (L.domPort routing "local", e1)
          e2 <- translateChain "mangle" "FORWARD"
          addEdge (L.domPort routing "forward", e2)
          e3 <- translateChain "raw" "OUTPUT"
          addEdge (outPort userspace, e3)

stateToLobster :: S -> [L.Decl]
stateToLobster S { sRules, sActions, sEdges } =
    ruleDecls ++ actionDecls ++ edgeDecls
  where ruleDecls = [ L.newDomain name "Rule" [fromString $ ppRuleOpts rule]
                    | (name, rule) <- Map.toList sRules
                    ]
        actionDecls = [ L.newDomain name "Action" [fromString action]
                      | (name, action) <- Map.toList sActions
                      ]
        ppRuleOpts rule =
          "\"" ++ (unwords . map printOption . rOptions $ rule) ++ "\""
        edgeDecls = [ l `L.right` r | (l, r) <- Set.toList sEdges ]

-- Note [Rule Names]: The convention for naming domains in Lobster
-- that correspond to iptables rules is a concatenation of the
-- enclosing filter name, chain name, and place which the rule appears
-- in the chain. For example, the first rule of the INPUT chain in the
-- filter table is called "filter_INPUT_0".

-- | A representation of things that aren't quite rules, but have some
-- effect, for example marking or logging a packet. These are named
-- based off of the matching portion of the rule that led to them
-- firing (see [Rule Names]).
type Action = String

-- | A left to right connection between domain ports
type Edge = (L.DomPort, L.DomPort)

-- | The state while we do the translation. We keep the whole rule
-- around so we can decide later how much info to put into the Lobster
-- metadata corresponding to it
data S = S { sRules       :: Map L.Name Rule
           -- ^ The rules we've encountered so far (see [Rule Names])
           , sActions     :: Map L.Name Action
           -- ^ The actions we've encountered so far
           , sEdges       :: Set Edge
           -- ^ The left-to-right edges we've encountered so far
           , sReturnPorts :: Map (String, String) (Set L.DomPort)
           -- ^ For each user chain, a collection of ports that return
           -- to the caller chain. Key is @(table, chain)@.
           , sSeenCalls :: Map String (Set (String, String))
           -- ^ For each table, a set of calls that we've seen from
           -- @(caller, callee)@. We keep track of this to avoid
           -- looping for cycles in rules.
           }
  deriving (Show)

data R = R { rIptables :: Iptables
           -- ^ The 'Iptables' we are translating
           , rTable :: String
           -- ^ The name of the current table (eg, "filter")
           , rChain :: String
           -- ^ The name of the current chain (eg, "INPUT")
           , rAccept :: L.DomPort
           -- ^ The port to connect to for @ACCEPT@-like targets that
           -- proceed to subsequent chains, tables, or devices
           }
  deriving (Show)

data Error
  -- | An unexpected error, probably indicating a bug in this
  -- translator
  = EUnexpected String
  -- | An error encountered while parsing the iptables rules
  | EIptablesParser ParseError
  -- | @EChainNotFound table chain@: a chain was not found that was
  -- either referred to explicitly by a jump or implicitly by the
  -- structure of the iptables system
  | EChainNotFound String String
  -- | A matching clause of a rule was not recognized (string is the
  -- iptables syntax for the clause)
  | EUnknownRule String
  -- | A target of a rule was not recognized (string is the iptables
  -- syntax for the target)
  | EUnknownTarget String
    deriving (Show)

newtype M a = M { unM :: EitherT Error (RWS R () S) a }
  deriving (Functor, Applicative, Monad, MonadReader R, MonadState S)

-- | Build an empty state.
initialS :: S
initialS = S Map.empty Map.empty Set.empty Map.empty Map.empty

runM :: Iptables -> String -> String -> L.DomPort -> M a -> (Either Error a, S)
runM ipts table chain accept m = (ea, s)
  where (ea, s, _) = runRWS (runEitherT $ unM m)
                            (R ipts table chain accept)
                            initialS

throwM :: Error -> M a
throwM = M . throwT

panic :: String -> M a
panic = throwM . EUnexpected

addRule :: L.Name -> Rule -> M ()
addRule name rule = modify $ \s -> s { sRules = Map.insert name rule (sRules s)}

-- | Add an 'Action' to the translation state
addAction :: L.Name -> Action -> M ()
addAction name action =
  modify $ \s -> s { sActions = Map.insert name (show action) (sActions s)}

addEdge :: Edge -> M ()
addEdge edge = modify $ \s -> s { sEdges = Set.insert edge (sEdges s)}

-- | Add a return port from the current chain
addReturnPort :: L.DomPort -> M ()
addReturnPort port = do
  R { rTable, rChain } <- ask
  modify $ \s ->
    s { sReturnPorts = Map.insertWith Set.union
                                      (rTable, rChain)
                                      (Set.singleton port)
                                      (sReturnPorts s)
      }

addSeenCall :: String -> (String, String) -> M ()
addSeenCall table (caller, callee) = modify $ \s ->
  s { sSeenCalls = Map.insertWith Set.union
                                  table
                                  (Set.singleton (caller, callee))
                                  (sSeenCalls s)
    }

-- | Translate a rule given incoming ports and the rule number in its
-- chain. Returns the incoming ports for the next rule in normal
-- order; this is usually just the @fail@ port of the current rule,
-- but for non-terminating rule targets like @LOG@, this might also
-- include the @match@ port. It may also be any return edges from
-- user-defined chains.
translateRule :: [L.DomPort] -> Integer -> Rule -> M [L.DomPort]
translateRule incs _num rule | uncondRule rule = do
  -- if this rule is unconditional, just wire incs up to the
  -- target, no fail edges since it can't fail
  outs <- forM incs $ \inc -> connectToTarget inc (rTarget rule)
  return (concat outs)
translateRule incs num rule = do
  R { rTable, rChain } <- ask
  let name = mkRuleName rTable rChain num
  addRule name rule
  forM_ incs $ \inc -> addEdge (inc, inPort name)
  outs <- connectToTarget (matchPort name) (rTarget rule)
  return ((failPort name):outs)

-- | Add edges to connect the given port to this rule target. The
-- return value is typically the empty list for targets that terminate
-- processing for this chain like @ACCEPT@ and @REJECT@. However if
-- the target is non-terminating like @LOG@ or is a custom chain that
-- will return to the next rule, we return those ports for subsequent
-- connection.
connectToTarget :: L.DomPort -> RuleTarget -> M [L.DomPort]
connectToTarget this target = do
  R { rTable, rAccept } <- ask
  case target of
    TAccept -> addEdge (this, rAccept) >> return []
    TDrop -> addEdge (this, drop) >> return []
    TReject _ -> addEdge (this, reject) >> return []
    TSecmark ctx -> do
      -- create an action to represent setting the security mark
      let baseName = fromMaybe "UNKNOWN" (L.portDomain this)
          name = baseName <> "_SECMARK"
      addAction name ("--selctx " ++ ctx)
      addEdge (this, inPort name)
      -- control flow continues
      return [outPort name]
    TConnsecmarkRestore -> do
      -- create an action to represent restoring the security mark
      let baseName = fromMaybe "UNKNOWN" (L.portDomain this)
          name = baseName <> "_CONNSECMARK"
      addAction name "--restore"
      addEdge (this, inPort name)
      -- control flow continues
      return [outPort name]
    TConnsecmarkSave -> do
      -- create an action to represent saving the security mark
      let baseName = fromMaybe "UNKNOWN" (L.portDomain this)
          name = baseName <> "_CONNSECMARK"
      addAction name "--save"
      addEdge (this, inPort name)
      -- control flow continues
      return [outPort name]
    TReturn -> do
      -- add the match port of this rule to the set of return ports
      -- for this chain
      addReturnPort this
      return []
    TUChain chain -> do
      -- translate the whole user chain. We have to do this first,
      -- otherwise we won't know where to make return edges from the
      -- called chain.
      mentry <- callChain rTable chain
      case mentry of
        -- call to empty chain: move to next rule and continue control flow
        Nothing -> return [this]
        Just entry -> do
          -- make the edge between this jump and the entry of the user chain
          addEdge (this, entry)
          returnPorts <- gets sReturnPorts
          let returns = fromMaybe Set.empty $ Map.lookup (rTable, chain) returnPorts
          -- return all of the return ports for the next rule
          return (Set.toList returns)
    _ -> throwM . EUnknownTarget $ printTarget target

-- | Like 'translateChain', but first checks whether we have already
-- translated this chain from this context. Used only for user-defined
-- chains. Returns 'Nothing' if the chain is empty.
callChain :: String -> String -> M (Maybe L.DomPort)
callChain table chain = do
  R { rTable, rChain } <- ask
  chain' <- lookupChain' table chain
  -- don't process empty user chains; they're a noop
  if null (cRules chain')
    then return Nothing
    else do
     -- this case should probably not happen, since this is only used
     -- for calling user-defined chains which by definition must be
     -- within the same table as the caller
     when (rTable /= table) $ panic "called user chain from wrong table"
     S { sSeenCalls } <- get
     case Map.lookup rTable sSeenCalls of
       Just calls | Set.member (rChain, chain) calls ->
         -- already seen, so just return the entrypoint
         return . Just . inPort $ mkRuleName rTable chain initialRuleNum
       _ -> do
         addSeenCall table (rChain, chain)
         Just <$> translateChain table chain

lookupChain :: String -> String -> M (Maybe Chain)
lookupChain table chain = do
  R { rIptables } <- ask
  chains <- lookupTable rIptables table
  return $ getChainByName chain chains

-- | Like 'lookupChain', but throws 'EChainNotFound' on failure.
lookupChain' :: String -> String -> M Chain
lookupChain' table chain = do
  mchain <- lookupChain table chain
  case mchain of
    Nothing -> throwM $ EChainNotFound table chain
    Just chain' -> return chain'

-- | @translateChain table chain incs@ translates the rules of the
-- specified chain in order. After this runs, the state will be
-- updated with the relevant rules and edges, as well as any return
-- ports found if this is a user-defined chain. The return value is
-- the entry port of this chain. This processes subsequent chains
-- transitively until it reaches non-iptables destinations like
-- network interfaces or userspace.
translateChain :: String -> String -> M L.DomPort
translateChain table chain = do
  R { rAccept } <- ask
  mchain <- lookupChain table chain
  maccept <- case nextChain table chain of
               Just (table', chain') ->
                 Just <$> translateChain table' chain'
               Nothing ->
                 return $ specialAccept table chain
  case mchain of
    Nothing ->
      -- sometimes we might look for a built-in chain and fail, but we
      -- can still figure out where the packets will flow with an
      -- missing chain, as that is equivalent to an empty chain with
      -- an ACCEPT policy. However we can't meaningfully translate an
      -- undefined user chain.
      case maccept of
        Nothing -> throwM $ EChainNotFound table chain
        Just accept -> return accept
    Just targetChain -> do
      let -- only modify accept target if we have one from
          -- above, otherwise we're in a user-defined chain and
          -- inherit our target
          accept = fromMaybe rAccept maccept
          switchEnv r =
            r { rTable = table
              , rChain = chain
              , rAccept = accept
              }
      local switchEnv $ do
        let exit = case cPolicy targetChain of
                     ACCEPT     -> return accept
                     DROP       -> return drop
                     -- user chain, so not applicable as we stop
                     -- before processing empty user chains
                     PUNDEFINED -> panic "shouldn't be translating empty user chain"
            entry  = mkRuleName table chain initialRuleNum
            loop [] incs _ | cPolicy targetChain == PUNDEFINED = do
              -- return edge for user chains handled through map
              forM_ incs $ \inc -> addReturnPort inc
              return (inPort entry)
            loop [] incs _ = do
              -- non-user chains proceed to exit
              ex <- exit
              forM_ incs $ \inc -> addEdge (inc, ex)
              return (inPort entry)
            loop (rule:rules) incs num = do
              incs' <- translateRule incs num rule
              loop rules incs' (num+1)
        case cRules targetChain of
          [] -> exit
          rules | all uncondRule rules -> do
            -- if we only have unconditional rules, we introduce a
            -- dummy rule as the entrypoint so that callers of this
            -- chain have something to jump to
            addRule entry (Rule (Counters 0 0) [] (TUnknown "synthetic entry rule" []))
            loop rules [(matchPort entry)] (initialRuleNum+1)
          rules -> loop rules [] initialRuleNum

-- | Returns @True@ if all of the options in this rule will
-- unconditionally succeed.
uncondRule :: Rule -> Bool
uncondRule rule = all uncondRuleOption (rOptions rule)

-- | Returns @True@ if this option unconditionally succeeds when
-- matching a packet (e.g., @--comment@).
uncondRuleOption :: RuleOption -> Bool
uncondRuleOption rOpt =
  case rOpt of
    OModule _  -> True
    OComment _ -> True
    _          -> False

-- | Encodes the destination for ACCEPT targets that lead to non-chain
-- destinations like network interfaces or userspace.
specialAccept :: String -> String -> Maybe L.DomPort
specialAccept table chain =
  lookup (chain, table) $
    [ (("PREROUTING" , "nat"     ), inPort routing)
    , (("INPUT"      , "security"), inPort userspace)
    , (("POSTROUTING", "nat"     ), outgoing)
    ]

-- | If a built-in chain has another chain after it in the processing
-- pipeline, return its table and chain. Returns 'Nothing' for a
-- user-defined chain, or for built-in chains that fall through to
-- devices or other destinations.
nextChain :: String -> String -> Maybe (String, String)
nextChain table chain =
  -- swap for readability of rules
  let swap (x,y) = (y,x)
  in swap <$> (lookup (chain, table) $
    [ (("PREROUTING", "raw"), ("PREROUTING", "mangle"))
    , (("PREROUTING", "mangle") , ("PREROUTING", "nat"))
    , (("INPUT", "mangle"), ("INPUT", "filter"))
    , (("INPUT", "filter"), ("INPUT", "security"))
    , (("FORWARD", "mangle"), ("FORWARD", "filter"))
    , (("FORWARD", "filter"), ("FORWARD", "security"))
    , (("FORWARD", "security"), ("POSTROUTING", "mangle"))
    , (("OUTPUT", "raw"), ("OUTPUT", "mangle"))
    , (("OUTPUT", "mangle"), ("OUTPUT", "nat"))
    , (("OUTPUT", "nat"), ("OUTPUT", "filter"))
    , (("OUTPUT", "filter"), ("OUTPUT", "security"))
    , (("OUTPUT", "security"), ("POSTROUTING", "mangle"))
    , (("POSTROUTING", "mangle"), ("POSTROUTING", "nat"))
    ])

-- | Look up a table by name. We panic here rather than having a
-- specialized error because there are no user-defined tables in
-- iptables.
lookupTable :: Iptables -> String -> M [Chain]
lookupTable ipts table =
  case table of
    "filter"   -> return $ tFilter ipts
    "nat"      -> return $ tNat ipts
    "mangle"   -> return $ tMangle ipts
    "raw"      -> return $ tRaw ipts
    "security" -> return $ tSecurity ipts
    _          -> panic $ "table not found: " ++ table

initialRuleNum :: Integer
initialRuleNum = 0

mkRuleName :: String -> String -> Integer -> L.Name
mkRuleName table chain num =
  fromString . concat $ [table, "_", chain, "_", show num]

inPort :: L.Name -> L.DomPort
inPort dom = L.domPort dom "in"

outPort :: L.Name -> L.DomPort
outPort dom = L.domPort dom "out"

matchPort :: L.Name -> L.DomPort
matchPort dom = L.domPort dom "match"

failPort :: L.Name -> L.DomPort
failPort dom = L.domPort dom "fail"

drop :: L.DomPort
drop = inPort "drop"

reject :: L.DomPort
reject = inPort "reject"

routing :: L.Name
routing = "routing"

userspace :: L.Name
userspace = "userspace"

incoming :: L.DomPort
incoming = L.DomPort Nothing "in"

outgoing :: L.DomPort
outgoing = L.DomPort Nothing "out"

preamble :: [L.Decl]
preamble = [
    L.newComment "An rule corresponding to a single rule in an iptables chain"
  , L.newClass "Rule" ["condition"]
      [ L.newComment "Incoming packet"
      , L.newPort "in"
      , L.newComment "Outgoing packet when condition is true"
      , L.newPort "match"
      , L.newComment "Outgoing packet when condition is false"
      , L.newPort "fail"
      ]
  , L.newComment "An action corresponding to an effectful target like LOG or MARK"
  , L.newClass "Action" ["action"]
      [ L.newComment "Incoming packet"
      , L.newPort "in"
      , L.newComment "Outgoing packet"
      , L.newPort "out"
      ]
  , L.newComment "Abstract representation of userspace"
  , L.newClass "UserSpace" []
      [ L.newComment "Packets entering userspace"
      , L.newPort "in"
      , L.newComment "Packets leaving userspace"
      , L.newPort "out"
      ]
  , L.newComment "Abstract representation of the routing table"
  , L.newClass "RoutingTable" []
      [ L.newComment "Incoming packets from nat PREROUTING"
      , L.newPort "in"
      , L.newComment "Outgoing packets to mangle INPUT"
      , L.newPort "local"
      , L.newComment "Outgoing packets to mangle FORWARD"
      , L.newPort "forward"
      ]
  , L.newComment "Abstract representation of a packet's destination outside of the current policy (eg DROP, REJECT)"
  , L.newClass "Destination" [] [L.newPort "in"]
  ]

-- | Create the abstract host with the given declarations added after
-- the standard set of @incoming@, @outgoing@, @forward@, @reject@,
-- @drop@, and @user@.
mkHost :: L.Name -> [L.Decl] -> L.Decl
mkHost name decls = L.newClass name [] (builtins ++ decls)
  where builtins = [ L.newComment "All incoming interfaces"
                   , L.newPort "in"
                   , L.newComment "All outgoing interfaces"
                   , L.newPort "out"
                   , L.newComment "Rejected packets flow here"
                   , L.newDomain "reject" "Destination" []
                   , L.newComment "Dropped packets flow here"
                   , L.newDomain "drop" "Destination" []
                   , L.newComment "This host's routing table"
                   , L.newDomain routing "RoutingTable" []
                   , L.newComment "This host's userspace"
                   , L.newDomain userspace "UserSpace" []
                   ]
