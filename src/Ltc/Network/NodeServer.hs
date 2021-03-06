{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Ltc.Network.NodeServer (
        module Ltc.Network.Types,

        nodePort,

        -- * Nodes and serving
        Node, shutdown,
        serve, serveFromLocation,

        -- * Handling types
        TypeHandler, handleType,

        -- * Connections and sending
        Connection, connect, closeConnection,
        sendMessage,

        -- * Neighbours
        addNeighbour, removeNeighbour
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO, threadDelay
                          , MVar, newMVar, withMVar, readMVar, modifyMVar, modifyMVar_ )
import Control.Exception ( Exception )
import Control.Monad ( unless, forever, forM, forM_, when, filterM )
import Control.Proxy ( Proxy, ProxyFast, Pipe, Producer, Consumer
                     , runProxy, lift, runIdentityP, request, respond, (>->) )
import Data.ByteString ( ByteString )
import Data.Default ( def )
import Data.List ( find )
import Data.Function ( on )
import Data.Map ( Map )
import Data.Maybe ( catMaybes )
import Data.Typeable ( Typeable )
import Language.Sexp ( printMach, toSexp )
import Ltc.Changeset ( Changeset(..), changesetBaseVersion
                     , Changes(..), changesToList
                     , WireDiff(..), diffFromWireDiff, wireDiffFromDiff )
import Ltc.Diff ( Diffable(..) )
import Ltc.Network.Interface ( NetworkInterface, Sending, Receiving, NetworkLocation )
import Ltc.Network.Interface.UDP ( UdpInterface )
import Ltc.Network.NodeProtocol ( NodeMessage(..), NodeEnvelope(..), encode, decode )
import Ltc.Store ( Store(..), Storable, SetCmd(..)
                 , Key, Version, NodeName
                 , Type, typeOf
                 , getExn )
import Network.BSD ( getHostName )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.VectorClock as VC
import qualified Ltc.Network.Interface as NI
import qualified Ltc.Network.Interface.UDP as U
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

-- Re-exported module
import Ltc.Network.Types

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "NodeServer"

----------------------
-- Node interface
----------------------

-- | The default node port.
nodePort :: Port
nodePort = 3582

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

data TypeHandler = TypeHandler
    { getSetCmdFromWireDiff :: forall s. (Store s) => s -> Key -> Version -> WireDiff -> IO SetCmd
    , getMergeWireDiffs     :: WireDiff -> WireDiff -> WireDiff
    , getType               :: Type
    }

-- | The abstract type of a network connection.  Note that it is parametrised by the type
-- of the network interface (e.g. 'UdpInterface').  Note also that since these connections
-- are asynchronous, there is generally no way to tell if the other side is running on
-- not.
data Connection a = (NetworkInterface a) => Connection
    { getConnectionInterface :: a Sending
    , getConnectionLocation  :: NetworkLocation a
    }

-- | The abstract type of a node.  You make a node with the @serve*@ functions, and you
-- close a node with 'shutdown'.
newtype Node a = Node { getNodeData :: MVar (NodeData a) }

-- | The local node's view of a remote node.
data RemoteNode a = NetworkInterface a => RemoteNode
    { getRemoteLocation  :: NetworkLocation a
    , getRemoteName      :: NodeName
    , getRemoteInterface :: a Sending
    , getRemoteClock     :: Version
    }

instance (NetworkInterface a) => Eq (RemoteNode a) where
    (==) = (==) `on` getRemoteLocation

instance (NetworkInterface a) => Ord (RemoteNode a) where
    compare = compare `on` getRemoteLocation

-- | The node's mutable state.
data NodeData a = NodeData
    { getShutdown       :: IO ()
    , getLocation       :: NetworkLocation a
    , getNodeName       :: NodeName
    , getNeighbours     :: Map NodeName (RemoteNode a)
    , getChangesetCache :: [(NodeName, Changeset)]
    , getTypeHandlers   :: [TypeHandler]
    }

-- | Start the node interface on the default port.
serve :: (Store s) => s -> NodeName -> IO (Node UdpInterface)
serve store nodeName = do
    hostname <- getHostName
    let location = U.UdpLocation { U.host = hostname
                                 , U.port = nodePort }
    serveFromLocation location store nodeName

-- | Start the node interface on the given port.
serveFromLocation :: (NetworkInterface a, Store s)
                  => NetworkLocation a -> s -> NodeName -> IO (Node a)
serveFromLocation location store nodeName = do
    debugM tag (printf "serveFromLocation %s" (show location))

    -- Make the node data-structure
    let nodeData = NodeData { getShutdown       = error "shutdown not yet defined"
                            , getLocation       = location
                            , getNodeName       = nodeName
                            , getNeighbours     = M.empty
                            , getChangesetCache = []
                            , getTypeHandlers   = []
                            }
    node <- Node <$> newMVar nodeData

    -- Start the incoming message handler
    tidNetwork <- forkIO $
        CE.bracket
            (NI.serve location)
            (\intf -> NI.close intf)
            (\intf -> CE.handle (\(_ :: Shutdown) -> return ())
                                (nodeHandler node store intf (interfaceReader intf)))

    -- Start the store event listener
    tidPublisher <- forkIO $ forever $ do
        threadDelay 2000000 -- 2s

        -- FIXME We should also send changes to neighbours when they're made to the store.
        sendChangesetsToNeighbours node store

    -- Fill in the 'getShutdown' field of the node
    modifyMVar_ (getNodeData node) $ \ndata ->
        return (ndata { getShutdown = mapM_ (flip CE.throwTo Shutdown)
                                            [tidNetwork, tidPublisher] })

    return node

-- | Shutdown a running 'Node'.  Idempotent.
shutdown :: (NetworkInterface a) => Node a -> IO ()
shutdown node = do
    withMVar (getNodeData node) $ \nodeData -> do
        debugM tag (printf "shutdown %s" (show (getNodeName nodeData)))
        getShutdown nodeData

-- | Use a local 'Node' to connect to a remote 'Node'.  Since this is an asynchronous
-- connection, we don't actually /connect/ to anything; we just get a handle for the
-- connection.
connect :: (NetworkInterface a) => Node a -> NetworkLocation a -> IO (Connection a)
connect _node location = do
    debugM tag (printf "connecting to %s" (show location))
    intf <- NI.connect location
    let conn = Connection { getConnectionInterface = intf
                          , getConnectionLocation  = location }
    debugM tag "connected"
    return conn

-- | Close a `Connection` to a remote `Node`.
closeConnection :: (NetworkInterface a) => Connection a -> IO ()
closeConnection conn = do
    debugM tag (printf "closing connection to %s" (show (getConnectionLocation conn)))
    NI.close (getConnectionInterface conn)
    debugM tag "closed connection"

-- | Send a single message from the local node on a connection to a remote node.
sendMessage :: (NetworkInterface a) => Node a -> Connection a -> NodeMessage -> IO ()
sendMessage node conn msg = do
    debugM tag (printf "sending message to %s" (show (getConnectionLocation conn)))
    nodeData <- readMVar (getNodeData node)
    let envelope = NodeEnvelope { getEnvelopeSender  = getLocation nodeData
                                , getEnvelopeNode    = getNodeName nodeData
                                , getEnvelopeMessage = msg
                                }
    NI.send (getConnectionInterface conn) (encode envelope)
    debugM tag "message sent"

-- | Add a type handler to the node for the given type.
handleType :: (Storable b)
           => Node a
           -> b                 -- ^ dummy value
           -> IO ()
handleType node dummy = do
    let typeHandler = TypeHandler { getType               = typeOf dummy
                                  , getMergeWireDiffs     = makeMergeWireDiffs dummy
                                  , getSetCmdFromWireDiff = makeSetCmdFromWireDiff dummy }
    modifyMVar_ (getNodeData node) $ \nodeData ->
        return (nodeData { getTypeHandlers = typeHandler : getTypeHandlers nodeData })

----------------------
-- Neighbour-set manipulation
----------------------

-- | Tell the given node that it has a neighbour at the given location.  Idempotent.
addNeighbour :: (NetworkInterface a) => Node a -> NodeName -> NetworkLocation a -> IO ()
addNeighbour node nodeName location = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        case M.lookup nodeName (getNeighbours nodeData) of
            Just _ -> do
                return nodeData
            Nothing -> do
                intf <- NI.connect location
                let remoteNode = RemoteNode { getRemoteLocation  = location
                                            , getRemoteName      = nodeName
                                            , getRemoteInterface = intf
                                            , getRemoteClock     = VC.empty
                                            }
                    neighbours' = M.insert nodeName remoteNode (getNeighbours nodeData)
                return (nodeData { getNeighbours = neighbours' })

-- | Tell the given node to not consider the node at the given location its neighbour
-- anymore.  Idempotent.
removeNeighbour :: (NetworkInterface a) => Node a -> NodeName -> IO ()
removeNeighbour node nodeName = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        let neighbours = getNeighbours nodeData
            mremoved = M.lookup nodeName neighbours
            neighbours' = M.delete nodeName neighbours
        case mremoved of
             Nothing      -> return ()
             Just removed -> NI.close (getRemoteInterface removed)
        return (nodeData { getNeighbours = neighbours' })

----------------------
-- Handler pipeline for incoming messages
----------------------

-- | Handle incoming node envelopes.
nodeHandler :: (NetworkInterface a, Store s)
            => Node a -> s -> a Receiving -> (() -> Producer ProxyFast ByteString IO ()) -> IO ()
nodeHandler node store intf p =
    runProxy $ p >-> envelopeDecoderD intf >-> handleNodeEnvelopeC node store

-- | Stream data from the network interface..
interfaceReader :: (Proxy p, NetworkInterface a) => a Receiving -> () -> Producer p ByteString IO ()
interfaceReader intf () = runIdentityP $ forever $ do
    bin <- lift $ NI.receive intf
    unless (BS.null bin) $ respond bin

-- | Decode envelopes and pass them downstream.  Malformed inputs are discarded, and a
-- warning is emitted.
envelopeDecoderD :: (NetworkInterface a, Proxy p)
                 => a Receiving -> () -> Pipe p ByteString (NodeEnvelope a) IO ()
envelopeDecoderD _intf () = runIdentityP $ forever $ do
    bin <- request ()
    case decode bin of
        Nothing       -> lift $ warningM tag "failed to decode envelope"
        Just envelope -> respond envelope

-- | Handle incoming node envelopes.
handleNodeEnvelopeC :: (Proxy p, Store s)
                    => Node a -> s -> () -> Consumer p (NodeEnvelope a) IO ()
handleNodeEnvelopeC node store () = runIdentityP $ forever $ do
    envelope <- request ()
    lift $ debugM tag (printf "handling %s"
                              (BL.unpack (printMach (toSexp (getEnvelopeMessage envelope)))))
    lift $ handleNodeEnvelope node store envelope

-- | Handle a single node envelope.
handleNodeEnvelope :: (Store s) => Node a -> s -> NodeEnvelope a -> IO ()
handleNodeEnvelope _node _store (NodeEnvelope {getEnvelopeMessage = Ping _}) = do
    debugM tag "ping handled"
handleNodeEnvelope node store envelope@(NodeEnvelope {getEnvelopeMessage = Change changeset}) = do
    -- Connect back to the sender.
    addNeighbour node (getEnvelopeNode envelope) (getEnvelopeSender envelope)

    -- Add the changeset to the node's changeset cache
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        let receivedVersion = getAfterVersion changeset
        haveChangeset <- hasVersion store receivedVersion
        if haveChangeset
           then do
               debugM tag "changeset already had"
               -- I have the changeset, so update our best guess of the remote node's
               -- state.
               let neighbours' = maybeUpdateRemoteClock (getNeighbours nodeData)
                                                        (getEnvelopeNode envelope)
                                                        receivedVersion
               return (nodeData { getNeighbours = neighbours' })
           else do
               debugM tag "new changeset"
               -- This is a new changeset, so add it to the cache.
               let changesetCache' = ((getEnvelopeNode envelope), changeset)
                                     : getChangesetCache nodeData
               return (nodeData { getChangesetCache = changesetCache' })

    -- Strictly speaking, this is unnecessary, but it's not slow and it cannot hurt.
    handleOwnedChangesets node store

    -- Try to apply changesets to the store
    tryApplyChangesets node store

    debugM tag "changes handled"
handleNodeEnvelope node store envelope@(NodeEnvelope {getEnvelopeMessage = Have receivedVersion}) = do
    -- Connect back to the sender.
    addNeighbour node (getEnvelopeNode envelope) (getEnvelopeSender envelope)

    -- Update our estimate of where the other node is.
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        haveChangeset <- hasVersion store receivedVersion
        if haveChangeset
           then do
               let neighbours' = maybeUpdateRemoteClock (getNeighbours nodeData)
                                                        (getEnvelopeNode envelope)
                                                        receivedVersion
               return (nodeData { getNeighbours = neighbours' })
           else do
               return nodeData

    debugM tag "have handled"

-- | Handle those 'Changeset's which we already have.  Drop the ones which are already in
-- the store.  Add the ones which are before the store's tip and then drop them.
handleOwnedChangesets :: (Store s) => Node a -> s -> IO ()
handleOwnedChangesets node store = do
    tip <- tipVersion store
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        changesets' <- flip filterM (getChangesetCache nodeData) $ \(_, changeset) -> do
            changesetPresent <- hasVersion store (getAfterVersion changeset)
            if not changesetPresent
                then do
                    if getAfterVersion changeset `VC.causes` tip
                        then do
                            addChangeset store changeset
                            return False
                        else do
                            return True
                else do
                    return False
        return (nodeData { getChangesetCache = changesets' })

-- | Try to apply as many changesets as possible to the data store.
tryApplyChangesets :: (Store s) => Node a -> s -> IO ()
tryApplyChangesets node store = do
    tryAgain <- withWriteLock store $ modifyMVar (getNodeData node) $ \nodeData -> do
        let changesets = getChangesetCache nodeData
        tip <- tipVersion store
        debugM tag (printf "trying to apply %d changesets to %s"
                           (length changesets)
                           (show tip))

        if length changesets > 0
            then do
                -- Attempt fast forward
                mFastForward <- findFastForward [] tip changesets
                case mFastForward of
                    Just ((remoteName, changeset), changesets') -> do
                        debugM tag (printf "fast-forwarding to %s" (show (getAfterVersion changeset)))
                        fastForward changeset (getTypeHandlers nodeData)
                        let neighbours' = maybeUpdateRemoteClock (getNeighbours nodeData)
                                                                 remoteName
                                                                 (getAfterVersion changeset)
                        return (nodeData { getChangesetCache = changesets'
                                         , getNeighbours     = neighbours' }, True)
                    Nothing -> do
                        debugM tag "not a fast-forward"
                        mMergeCandidate <- findMergeCandidate [] changesets
                        case mMergeCandidate of
                            Just ((remoteName, changeset), changesets') -> do
                                debugM tag (printf "merging %s x %s"
                                                   (show tip)
                                                   (show (getAfterVersion changeset)))
                                mergeIntoHistory changeset (getTypeHandlers nodeData)
                                tip' <- tipVersion store
                                debugM tag (printf "tip after merge %s" (show tip'))
                                let neighbours' = maybeUpdateRemoteClock (getNeighbours nodeData)
                                                                         remoteName
                                                                         (getAfterVersion changeset)
                                return (nodeData { getChangesetCache = changesets'
                                                 , getNeighbours     = neighbours' }, True)
                            Nothing -> do
                                debugM tag "not a merge"
                                return (nodeData, False)
            else do
                return (nodeData, False)
    -- If we've made reduced the number of cached changesets, try to apply more.
    when tryAgain $ tryApplyChangesets node store
  where
    -- | Find the first 'Changeset' that begins in the given version.
    findFastForward :: [(NodeName, Changeset)]
                    -> Version
                    -> [(NodeName, Changeset)]
                    -> IO (Maybe ((NodeName, Changeset), [(NodeName, Changeset)]))
    findFastForward _ _ [] =
        return Nothing
    findFastForward acc tip ((remoteName, changeset) : changesets) = do
        case changeset of
            Update { getBeforeUpdateVersion = beforeVersion } | beforeVersion == tip -> do
                return (Just ((remoteName, changeset), reverse acc ++ changesets))
            Merge {} -> do
                hasAncestor <- hasVersion store (getMergeAncestorVersion changeset)
                -- If the merge result is based on a version we already have, and if it's
                -- in the future, it's a "fast-forward" merge.
                if hasAncestor && tip `VC.causes` getAfterVersion changeset
                    then do
                        -- FIXME Warning: Applying only this will leave holes in the history.
                        return (Just ((remoteName, changeset), reverse acc ++ changesets))
                    else do
                        findFastForward ((remoteName, changeset) : acc) tip changesets
            _ -> do
                findFastForward ((remoteName, changeset) : acc) tip changesets

    -- | Find the first 'Changeset' that extends a 'Changeset' already in the store.
    findMergeCandidate :: [(NodeName, Changeset)]
              -> [(NodeName, Changeset)]
              -> IO (Maybe ((NodeName, Changeset), [(NodeName, Changeset)]))
    findMergeCandidate _ [] =
        return Nothing
    findMergeCandidate acc ((remoteName, changeset) : changesets) = do
        -- Note that in the case of a 'Merge', we have already handled the case where it
        -- extends the tip with fast-forwards.
        hasAncestor <- hasVersion store (changesetBaseVersion changeset)
        if hasAncestor
            then do
                return (Just ((remoteName, changeset), reverse acc ++ changesets))
            else do
                findMergeCandidate ((remoteName, changeset) : acc) changesets

    -- | Apply the given 'Changeset'.  It better be a fast-forward.
    fastForward :: Changeset -> [TypeHandler] -> IO ()
    fastForward changeset typeHandlers = do
        cmds <- setCmdsFromChangeset store changeset typeHandlers
        _ <- msetInternal store changeset cmds True
        return ()

    -- | Merge the given 'Changeset' into the history by creating a new tip 'Changeset'.
    mergeIntoHistory :: Changeset -> [TypeHandler] -> IO ()
    mergeIntoHistory changeset typeHandlers = do
        changesets <- changesetsAfter store (changesetBaseVersion changeset)

        -- FIXME Merge operations should be atomic.
        -- Apply the give changeset.
        cmds <- setCmdsFromChangeset store changeset typeHandlers
        _ <- msetInternal store changeset cmds True

        -- Formulate the merge.
        mergeChangeset <- mergeFromChangesets (changesetBaseVersion changeset)
                                              changesets
                                              [changeset]
                                              typeHandlers
        debugM tag (printf "merged changeset: %s" (show mergeChangeset))
        mergeCmds <- setCmdsFromChangeset store mergeChangeset typeHandlers
        _ <- msetInternal store mergeChangeset mergeCmds True

        return ()

maybeUpdateRemoteClock :: Map NodeName (RemoteNode a)
                       -> NodeName
                       -> Version
                       -> Map NodeName (RemoteNode a)
maybeUpdateRemoteClock neighbours remoteName clock' =
    M.adjust (\remoteNode ->
               let clock = getRemoteClock remoteNode
               in remoteNode { getRemoteClock = if clock `VC.causes` clock'
                                                then clock'
                                                else clock })
             remoteName
             neighbours

----------------------
-- Changeset deserialization
----------------------

-- | Convert a 'Changeset' to a list of 'SetCmd's using the given 'TypeHandler's.
setCmdsFromChangeset :: (Store s) => s -> Changeset -> [TypeHandler] -> IO [SetCmd]
setCmdsFromChangeset store changeset typeHandlers = do
    let changes = changesToList (getChanges changeset)
    catMaybes <$> forM changes (\(key, wireDiff) -> do
        let mTypeHandler =
                find (\handler -> getType handler == getWireDiffType wireDiff)
                     typeHandlers
        case mTypeHandler of
            Nothing -> do
                warningM tag (printf "no type handler for %s" (show (getWireDiffType wireDiff)))
                return Nothing
            Just typeHandler -> do
                Just <$> (getSetCmdFromWireDiff typeHandler) store
                                                             key
                                                             (changesetBaseVersion changeset)
                                                             wireDiff)

-- | Combine the given 'Changeset's into a single one based on the given 'Version', and
-- return the merged 'Changeset'.
mergeFromChangesets :: Version -> [Changeset] -> [Changeset] -> [TypeHandler] -> IO Changeset
mergeFromChangesets version changesetsLeft changesetsRight typeHandlers = do
    let versionLeft  = getAfterVersion (last changesetsLeft)
        versionRight = getAfterVersion (last changesetsRight)
        version' = VC.max versionLeft versionRight
        changes = Changes (foldl combineChangesets M.empty (changesetsLeft ++ changesetsRight))
    return (Merge { getBeforeMergeVersions  = (versionLeft, versionRight)
                  , getMergeAncestorVersion = version
                  , getAfterVersion         = version'
                  , getChanges              = changes
                  })
  where
    -- | Combine the 'Changes' in a list of 'Changeset's.
    combineChangesets :: Map Key WireDiff -> Changeset -> Map Key WireDiff
    combineChangesets changes changeset =
        foldl addWireDiffToChanges changes (changesToList (getChanges changeset))

    addWireDiffToChanges :: Map Key WireDiff -> (Key, WireDiff) -> Map Key WireDiff
    addWireDiffToChanges changes (key, wireDiff) =
        let mTypeHandler =
                find (\handler -> getType handler == getWireDiffType wireDiff)
                     typeHandlers
        in case mTypeHandler of
            Nothing ->
                -- error (printf "no type handler for %s" (show (getWireDiffType wireDiff)))
                changes
            Just typeHandler ->
                M.insertWith (getMergeWireDiffs typeHandler) key wireDiff changes

-- | Make a function that transforms part of a 'WireDiff' into a `SetCmd`.
makeSetCmdFromWireDiff :: forall a s. (Storable a, Store s)
                 => a           -- ^ dummy value to fix the type
                 -> (s -> Key -> Version -> WireDiff -> IO SetCmd)
makeSetCmdFromWireDiff _ = \store key version wireDiff -> do
    mtyp <- keyType store key
    case mtyp of
        Nothing -> do
            -- The key does not exist, so just assume 'def' for the previous value.
            let v = def :: a
            return (setCmdFromWireDiff key v wireDiff)
        Just typ -> do
            if typ == getWireDiffType wireDiff
                then do
                    (v :: a) <- getExn store key version
                    return (setCmdFromWireDiff key v wireDiff)
                else do
                    error "setCmdFromWireDiff applied to wrong type"
  where
    -- | Get a `SetCmd` from a 'WireDiff'.
    setCmdFromWireDiff :: Key -> a -> WireDiff -> SetCmd
    setCmdFromWireDiff key v wireDiff =
        let Just diff = diffFromWireDiff wireDiff
        in SetCmd key (applyDiff v diff)

-- | Make a function that merges two 'WireDiff's.
makeMergeWireDiffs :: forall a. (Storable a)
                   => a         -- ^ dummy value to fix the type
                   -> (WireDiff -> WireDiff -> WireDiff)
makeMergeWireDiffs _ = \wireDiff1 wireDiff2 ->
    case (diffFromWireDiff wireDiff1, diffFromWireDiff wireDiff2) of
        (Just (diff1 :: Diff a), Just (diff2 :: Diff a)) ->
            wireDiffFromDiff (mergeDiffs diff1 diff2)
        _ ->
            error "mergeWireDiffs applied to wrong type"

----------------------
-- Change propagation
----------------------

sendChangesetsToNeighbours :: (NetworkInterface a, Store s) => Node a -> s -> IO ()
sendChangesetsToNeighbours node store = do
    withMVar (getNodeData node) $ \nodeData -> do
        mapM_ (sendChangesetsToNeighbour nodeData) (M.elems (getNeighbours nodeData))
  where
    sendChangesetsToNeighbour nodeData remoteNode = do
        changesets <- changesetsNotBefore store (getRemoteClock remoteNode)
        debugM tag (printf "sending %d changesets to neighbour %s (est. %s)"
                           (length changesets)
                           (show (getRemoteName remoteNode))
                           (show (getRemoteClock remoteNode)))
        -- Send the changes we think the other node doesn't have.
        forM_ changesets $ \changeset -> do
            let envelope = NodeEnvelope { getEnvelopeSender  = getLocation nodeData
                                        , getEnvelopeNode    = getNodeName nodeData
                                        , getEnvelopeMessage = Change changeset
                                        }
            NI.send (getRemoteInterface remoteNode) (encode envelope)
        -- If we don't have any changes to send, send an update saying what our most
        -- recent state is.
        when (length changesets == 0) $ do
            tip <- tipVersion store
            let envelope = NodeEnvelope { getEnvelopeSender  = getLocation nodeData
                                        , getEnvelopeNode    = getNodeName nodeData
                                        , getEnvelopeMessage = Have tip
                                        }
            NI.send (getRemoteInterface remoteNode) (encode envelope)
