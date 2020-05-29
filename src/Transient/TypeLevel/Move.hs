

{-# LANGUAGE CPP, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts  #-}
module Transient.TypeLevel.Move where

-- module Transient.TypeLevel.Move where
import Transient.TypeLevel.Effects
import Transient.Logged(Loggable)
import qualified Transient.Move as Tr
import qualified Transient.Mailboxes as Tr
import qualified Data.ByteString.Lazy.Char8             as BS
import qualified Control.Monad.Trans as T

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

connect ::  Tr.Node ->  Tr.Node -> T '[Cloud] ()
connect n n' = liftCl $ Tr.connect n n'

connect' :: Tr.Node -> T '[Cloud] ()
connect' = liftCl . Tr.connect' 

listen :: Tr.Node -> T '[] ()
listen = liftCl . Tr.listen

-- Low level 
addNodes :: [Tr.Node] ->  T '[] ()
addNodes= TR . Tr.addNodes


addThisNodeToRemote :: T '[Cloud] ()
addThisNodeToRemote = liftCl Tr.addThisNodeToRemote

shuffleNodes :: T '[] [Tr.Node]
shuffleNodes= TR Tr.shuffleNodes




-- Connection(..), ConnectionData(..), defConnection,

-- ** Querying nodes

getMyNode :: T '[] Tr.Node
getMyNode = TR Tr.getMyNode

getWebServerNode :: T '[] Tr.Node
getWebServerNode = TR Tr.getWebServerNode


getNodes :: T '[] [Tr.Node]
getNodes= TR Tr.getNodes

nodeList= Tr.nodeList

isBrowserInstance= Tr.isBrowserInstance


-- * Running Local Computations
local :: Loggable a => T effs a -> T (effs <: Logged) a
local (TR mx)= liftCl $ Tr.local mx

onAll :: T effs a -> T effs a 
onAll (TR mx)= liftCl $ Tr.onAll mx

lazy :: T effs a -> T effs a 
lazy (TR mx)= liftCl $ Tr.lazy mx

localFix :: T '[] ()
localFix = liftCl Tr.localFix

fixRemote :: T effs a -> T effs a
fixRemote (TR mx)= liftCl $ Tr.fixRemote mx

loggedc :: Loggable a => T effs a -> T (effs <: Logged) a
loggedc (TR mx)= liftCl $ Tr.local mx

lliftIO :: Loggable a => IO a -> T ('[IOEff] <: Logged) a
lliftIO  mx = local $ liftIO mx

localIO :: Loggable a => IO a -> T ('[IOEff] <: Logged) a
localIO= lliftIO 

-- * Moving Computations
wormhole :: (Loggable a, Member Cloud effs ~ True) => Tr.Node -> T  effs a -> T effs a
wormhole n (TR mx) = liftCl $ Tr.wormhole n $ Tr.Cloud mx

teleport :: T '[Cloud] ()
teleport = liftCl Tr.teleport

copyData :: Loggable a => a -> T '[State a] a
copyData = liftCl . Tr.copyData

fixClosure :: T '[Cloud] ()
fixClosure= liftCl Tr.fixClosure

-- * Running at a Remote Tr.Node
beamTo :: Tr.Node -> T '[Cloud] ()
beamTo= liftCl . Tr.beamTo


forkTo :: Tr.Node -> T '[Cloud] ()
forkTo = liftCl . Tr.forkTo

callTo :: (Loggable a, Member Logged effs ~ True) 
       => Tr.Node 
       -> T effs a 
       -> T (effs <: Cloud) a
callTo n (TR mx)=  liftCl $ Tr.callTo n $ Tr.Cloud mx

runAt :: (Loggable a, Member Logged effs ~ True) 
       => Tr.Node 
       -> T effs a 
       -> T (effs <: Cloud) a
runAt= callTo

atRemote :: (Loggable a,Member Logged effs ~ True) 
         => T effs a 
         -> T (effs <: Cloud) a
atRemote (TR mx)= liftCl $ Tr.atRemote $ Tr.Cloud mx

setSynchronous :: Bool -> T '[] ()
setSynchronous = TR. Tr.setSynchronous

syncStream :: Member Cloud effs ~ True => T effs a -> T effs a
syncStream (TR mx)= liftCl $ Tr.syncStream (Tr.Cloud mx)


-- * Running at Multiple Nodes
clustered :: (Loggable a, Member Cloud effs ~ True)  => T effs a -> T effs  a
clustered (TR mx)= liftCl $ Tr.clustered $ Tr.Cloud mx 

mclustered :: (Monoid a, Loggable a,Member Cloud effs ~ True)  
           => T effs a -> T effs  a 
mclustered (TR mx)= liftCl $ Tr.mclustered $ Tr.Cloud mx 

callNodes :: Loggable a
          => (T effs a -> T effs' a -> T (effs :++ effs') a)
          -> T effs'' a -> T effs''' a -> T (effs :++ effs' :++ effs'' :++ effs''') a
callNodes op (TR init) (TR proc)= liftCl $ Tr.callNodes op' (Tr.Cloud init) (Tr.Cloud proc) 
  where
  op' (Tr.Cloud x) (Tr.Cloud y)= let TR r= op (TR x)  (TR y) in Tr.Cloud r 
  
  
-- * Messaging
putMailbox :: Typeable val => val -> T '[] ()
putMailbox = TR . Tr.putMailbox 

putMailbox' :: (Typeable key, Ord key, Typeable val) =>  key -> val -> T '[] ()
putMailbox' box v= TR $ Tr.putMailbox' box v

getMailbox :: Typeable val => T '[] val
getMailbox = TR Tr.getMailbox

getMailbox' :: (Typeable key, Ord key, Typeable val) => key -> T '[] val
getMailbox' = TR . Tr.getMailbox'

deleteMailbox :: Typeable a => a -> T '[] ()
deleteMailbox = TR . Tr.deleteMailbox


-- * Thread Control
single :: T effs a -> T effs a
single (TR mx)= TR $ Tr.single mx

unique :: T effs a -> T effs a
unique (TR mx)= TR $ Tr.unique mx

#ifndef ghcjs_HOST_OS
-- * Buffering Control
setBuffSize  :: Int -> T '[] ()
setBuffSize= TR . Tr.setBuffSize

getBuffSize :: T '[] Int
getBuffSize= TR Tr.getBuffSize
#endif

#ifndef ghcjs_HOST_OS
-- * REST API
api :: T effs BS.ByteString -> T (effs <: Cloud) ()
api (TR mx)= TR $ let Tr.Cloud x= Tr.api mx in x

#endif

-- module Transient.Move (HTTPMethod(..),PostParams)
