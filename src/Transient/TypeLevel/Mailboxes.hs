{-# LANGUAGE CPP, DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}

module Transient.TypeLevel.Mailboxes(
putMailbox,putMailbox', getMailbox, getMailbox', deleteMailbox, deleteMailbox'
) where

import Transient.TypeLevel.Effects
import qualified Transient.Mailboxes as Ma
import Data.Typeable





-- | write to the mailbox
-- Mailboxes are node-wide, for all processes that share the same connection data, that is, are under the
-- same `listen`  or `connect`
-- while EVars are only visible by the process that initialized  it and his children.
-- Internally, the mailbox is in a well known EVar stored by `listen` in the `Connection` state.
putMailbox :: Typeable val => val -> T '[] '[] ()
putMailbox = TR . Ma.putMailbox

-- | write to a mailbox identified by an identifier besides the type
putMailbox' :: (Typeable key, Ord key, Typeable val) =>  key -> val -> T '[] '[] ()
putMailbox' k = TR . Ma.putMailbox' k

-- | get messages from the mailbox that matches with the type expected.
-- The order of reading is defined by `readTChan`
-- This is reactive. it means that each new message trigger the execution of the continuation
-- each message wake up all the `getMailbox` computations waiting for it.
getMailbox :: Typeable val => T '[] '[Async,Streaming] val
getMailbox=  TR  Ma.getMailbox

-- | read from a mailbox identified by an identifier besides the type
getMailbox' :: (Typeable key, Ord key, Typeable val) => key -> T '[] '[Async,Streaming] val
getMailbox' =  TR  . Ma.getMailbox'


-- | delete all subscriptions for that mailbox expecting this kind of data
deleteMailbox :: Typeable a => a -> T '[] '[] ()
deleteMailbox= TR . Ma.deleteMailbox

-- | clean a mailbox identified by an Int and the type
deleteMailbox' :: (Typeable key, Ord key, Typeable a) => key ->  a -> T '[] '[] ()
deleteMailbox' k = TR . Ma.deleteMailbox' k