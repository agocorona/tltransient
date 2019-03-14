

{-# LANGUAGE CPP, DataKinds, TypeOperators, FlexibleContexts  #-}
module Transient.TypeLevel.Base where

-- module Transient.TypeLevel.Move where
import Transient.TypeLevel.Effects
import Transient.Logged(Loggable)
import qualified Transient.Move as Tr
import qualified Data.ByteString.Lazy.Char8             as BS
import Data.Type.Set
import Data.Typeable

-- * Node & Cluster Management
-- $cluster

-- module Transient.Move(Tr.Node(..))

-- ** Creating nodes

-- module Transient.Move(Tr.Service())

createNodeServ= Tr.createNodeServ

createNode= Tr.createNode

createWebNode= Tr.createWebNode

-- ** Joining the cluster

connect ::  Tr.Node ->  Tr.Node -> TRS '[Cloud] ()
connect n n' = liftCl $ Tr.connect n n'

connect' :: Tr.Node -> TRS '[Cloud] ()
connect' = liftCl . Tr.connect' 

listen :: Tr.Node -> TRS '[] ()
listen = liftCl . Tr.listen

-- Low level 
addNodes :: [Tr.Node] ->  TRS '[] ()
addNodes= TR . Tr.addNodes


addThisNodeToRemote :: TRS '[Cloud] ()
addThisNodeToRemote = liftCl Tr.addThisNodeToRemote

shuffleNodes :: TRS '[] [Tr.Node]
shuffleNodes= TR Tr.shuffleNodes




-- Connection(..), ConnectionData(..), defConnection,

-- ** Querying nodes

getMyNode :: TRS '[] Tr.Node
getMyNode = TR Tr.getMyNode

getWebServerNode :: TRS '[] Tr.Node
getWebServerNode = TR Tr.getWebServerNode


getNodes :: TRS '[] [Tr.Node]
getNodes= TR Tr.getNodes

nodeList= Tr.nodeList

isBrowserInstance= Tr.isBrowserInstance


-- * Running Local Computations
local :: Loggable a => TRS effs a -> TRS (effs :++ '[Logged]) a
local (TR mx)= liftCl $ Tr.local mx

onAll :: TRS effs a -> TRS effs a 
onAll (TR mx)= liftCl $ Tr.onAll mx

lazy :: TRS effs a -> TRS effs a 
lazy (TR mx)= liftCl $ Tr.lazy mx

localFix :: TRS '[] ()
localFix = liftCl Tr.localFix

fixRemote :: TRS effs a -> TRS effs a
fixRemote (TR mx)= liftCl $ Tr.fixRemote mx

loggedc :: Loggable a => TRS effs a -> TRS (effs :++ '[Logged]) a
loggedc (TR mx)= liftCl $ Tr.local mx

lliftIO :: Loggable a => IO a -> TRS '[IOEff,Logged] a
lliftIO  mx = local $ liftIO mx

localIO :: Loggable a => IO a -> TRS '[IOEff,Logged] a
localIO= lliftIO 

-- * Moving Computations
wormhole :: (Loggable a, Member Cloud effs) => Tr.Node -> TRS  effs a -> TRS effs a
wormhole n (TR mx) = liftCl $ Tr.wormhole n $ Tr.Cloud mx

teleport :: TRS '[Cloud] ()
teleport = liftCl Tr.teleport

copyData :: Loggable a => a -> TRS '[State a] a
copyData = liftCl . Tr.copyData

fixClosure :: TRS '[Cloud] ()
fixClosure= liftCl Tr.fixClosure

-- * Running at a Remote Tr.Node
beamTo :: Tr.Node -> TRS '[Cloud] ()
beamTo= liftCl . Tr.beamTo


forkTo :: Tr.Node -> TRS '[Cloud] ()
forkTo = liftCl . Tr.forkTo

callTo :: (Loggable a, Member Logged effs) 
       => Tr.Node 
       -> TRS effs a 
       -> TRS (effs :++ '[Cloud]) a
callTo n (TR mx)=  liftCl $ Tr.callTo n $ Tr.Cloud mx

runAt :: (Loggable a, Member Logged effs) 
       => Tr.Node 
       -> TRS effs a 
       -> TRS (effs :++ '[Cloud]) a
runAt= callTo

atRemote :: (Loggable a,Member Logged effs) 
         => TRS effs a 
         -> TRS (effs :++ '[Cloud]) a
atRemote (TR mx)= liftCl $ Tr.atRemote $ Tr.Cloud mx

setSynchronous :: Bool -> TRS '[] ()
setSynchronous = TR. Tr.setSynchronous

syncStream :: Member Cloud effs => TRS effs a -> TRS effs a
syncStream (TR mx)= liftCl $ Tr.syncStream (Tr.Cloud mx)


-- * Running at Multiple Nodes
clustered :: (Loggable a, Member Cloud effs)  => TRS effs a -> TRS effs  a
clustered (TR mx)= liftCl $ Tr.clustered $ Tr.Cloud mx 

mclustered :: (Monoid a, Loggable a,Member Cloud effs)  
           => TRS effs a -> TRS effs  a 
mclustered (TR mx)= liftCl $ Tr.mclustered $ Tr.Cloud mx 

callNodes :: Loggable a
          => (TRS effs a -> TRS effs' a -> TRS (effs :++ effs') a)
          -> TRS effs'' a -> TRS effs''' a -> TRS (effs :++ effs' :++ effs'' :++ effs''') a
callNodes op (TR init) (TR proc)= liftCl $ Tr.callNodes op' (Tr.Cloud init) (Tr.Cloud proc) 
  where
  op' (Tr.Cloud x) (Tr.Cloud y)= let TR r= op (TR x)  (TR y) in Tr.Cloud r 
  
  
-- * Messaging
putMailbox :: Typeable val => val -> TRS '[] ()
putMailbox = TR . Tr.putMailbox 

putMailbox' :: (Typeable key, Ord key, Typeable val) =>  key -> val -> TRS '[] ()
putMailbox' box v= TR $ Tr.putMailbox' box v

getMailbox :: Typeable val => TRS '[] val
getMailbox = TR Tr.getMailbox

getMailbox' :: (Typeable key, Ord key, Typeable val) => key -> TRS '[] val
getMailbox' = TR . Tr.getMailbox'

cleanMailbox :: Typeable a => a -> TRS '[] ()
cleanMailbox = TR . Tr.cleanMailbox

cleanMailbox' :: (Typeable key, Ord key, Typeable a) => key ->  a -> TRS '[] ()
cleanMailbox' box v = TR $ Tr.cleanMailbox' box v

-- * Thread Control
single :: TRS effs a -> TRS effs a
single (TR mx)= TR $ Tr.single mx

unique :: TRS effs a -> TRS effs a
unique (TR mx)= TR $ Tr.unique mx

#ifndef ghcjs_HOST_OS
-- * Buffering Control
setBuffSize  :: Int -> TRS '[] ()
setBuffSize= TR . Tr.setBuffSize

getBuffSize :: TRS '[] Int
getBuffSize= TR Tr.getBuffSize
#endif

#ifndef ghcjs_HOST_OS
-- * REST API
api :: TRS effs BS.ByteString -> TRS (effs :++ '[Cloud]) ()
api (TR mx)= TR $ let Tr.Cloud x= Tr.api mx in x

#endif

-- module Transient.Move (HTTPMethod(..),PostParams)
