{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}
module Transient.TypeLevel.Base where

import Prelude
import qualified Transient.Base as Tr
import Transient.TypeLevel.Effects
import Data.Typeable
import Control.Exception


-- Effects  of this module
data State a 
data RState a 

data Throws e
data Handle e
data Terminal



-- * Composition Operators
(**>) :: T req effs a 
      -> T req' effs'  b 
      -> T (req :++ req')  (effs :++  effs')   b
(**>) (TR x) (TR y) = TR $ x Tr.**> y



(<**) :: T req effs a 
      -> T req' effs'  b 
      -> T (req :++ req')  (effs :++  effs')   a
(<**) (TR x) (TR y) = TR $ x Tr.<** y

(<***) :: T req effs a  
       -> T req' effs'  b 
       -> T (req :++ req')  (effs :++  effs')   a
(<***) (TR x) (TR y) = TR $ x Tr.<*** y


-- * Running the monad
keep :: Typeable a => T '[] (  effs) a -> IO (Maybe a)
keep (TR x) = Tr.keep x 

keep' :: Typeable a => T '[] (  effs) a -> IO (Maybe a)
keep' (TR x) = Tr.keep' x 

stop = empty

exit :: Typeable a => a -> T '[] '[] a
exit = TR . Tr.exit 

-- * Asynchronous console IO

option :: (Typeable b, Show b, Read b, Eq b)
       => b -> String  
       -> T '[] '[Terminal]  b
option ret prompt= TR $ Tr.option ret prompt

input :: (Typeable a, Read a,Show a) 
      =>  (a -> Bool) 
      -> String 
      -> T '[] '[Terminal]  a
input val prompt= TR  $ Tr.input val prompt

input' :: (Typeable a, Read a,Show a) => Maybe a -> (a -> Bool) -> String -> T '[] '[Terminal]  a
input' ma val prompt= TR $ Tr.input' ma val prompt



-- * Task Creation


parallel :: IO (Tr.StreamData b) -> T '[] '[MThread,Streaming] (Tr.StreamData b)
parallel= TR. Tr.parallel

async ::  IO a -> T '[] '[Async,MThread]  a
async = TR . Tr.async 


waitEvents :: IO a -> T '[]  '[Async,Streaming,MThread]  a
waitEvents = TR . Tr.waitEvents

spawn :: IO a -> T '[]  '[Streaming,MThread] a
spawn = TR . Tr.waitEvents

sample :: Eq a => IO a -> Int -> T '[] '[Streaming,MThread] a
sample f n= TR $ Tr.sample f n

abduce :: T '[] '[Async,MThread]  ()
abduce = TR Tr.abduce

-- * State management
set :: Typeable a => a -> T '[]  '[State a]  ()
set = TR . Tr.setData



get :: Typeable a => T '[State a] '[] a
get = TR Tr.getSData  


setData :: Typeable a => a -> T '[]  '[State a]  ()
setData= set

getSData ::  Typeable a => T '[] '[Maybe Terminates]  a
getSData = TR  Tr.getSData

getData :: Typeable a => T '[] '[] (Maybe a)
getData = TR Tr.getData

delData  :: Typeable a => a -> T '[] '[] ()
delData x= TR $ Tr.delData x

modifyData :: Typeable a => (Maybe a -> Maybe a) -> T '[] '[] ()  --- problematic!!!!
modifyData = TR .Tr.modifyData

modifyData' :: Typeable a => (a -> a) ->  a -> T '[] '[State a] a
modifyData' f x = TR $ Tr.modifyData' f x

try :: T req effs a -> T req effs a
try (TR mx)= TR $ Tr.try mx

setState :: Typeable a => a -> T '[]  '[State a]  ()
setState= setData

getState ::  Typeable a => T '[] '[Maybe  Terminates]  a
getState= getSData

delState :: Typeable a => a -> T '[] '[] ()
delState= delData

modifyState :: Typeable a => (Maybe a -> Maybe a) -> T '[] '[] ()
modifyState= modifyData

getRState :: Typeable a => T '[] '[Maybe Terminates]  a
getRState = TR Tr.getRState

setRState :: Typeable a => a -> T '[] '[RState a] ()
setRState = TR . Tr.setRState



-- * Thread management

threads0 :: T req effs  a -> T req (effs :\ MThread)  a
threads0 (TR x)= TR $ Tr.threads 0 x

threads :: Int -> T req effs a -> T req effs a
threads n (TR mx)= TR $ Tr.threads n mx

addThreads :: Int ->  T '[] '[MThread] ()
addThreads = TR . Tr.addThreads

freeThreads :: T req effs a -> T req ( MThread :> effs) a
freeThreads (TR mx)= TR $ Tr.freeThreads mx

hookedThreads :: T req effs a -> T req effs a
hookedThreads (TR mx)= TR $ Tr.hookedThreads mx

oneThread :: T req effs a -> T req (effs :\ MThread) a
oneThread (TR mx)= TR $ Tr.oneThread mx

killChilds :: T '[] '[] ()
killChilds = TR  Tr.killChilds

-- * Exceptions
-- $exceptions

onException :: Exception e => (e -> T req effs a) -> T req (effs <: Handle e) ()
onException exc= TR $ Tr.onException $ \e ->  let TR r= exc e in r Prelude.>> Prelude.return ()

onException' :: Exception e 
             => T req effs a 
             -> (e -> T req' effs' a) 
             -> T (req :++ req') (effs :++  effs'  <: Handle e) a
onException' (TR x) exc = TR $ x `Tr.onException'` \e -> let TR r= exc e in r

cutExceptions :: T '[] '[] ()
cutExceptions= TR Tr.cutExceptions

continue :: T '[] '[] ()
continue = TR $ Tr.continue

catcht :: Exception e => T req effs b -> (e -> T req' effs' b) -> T (req :++ req') (effs :++ effs' <: Handle e) b
catcht (TR x) (exc) = TR $ x `Tr.catcht` \e -> let TR r= exc e in  r 

throw ::  Exception e => e -> T '[] '[Throws e] a
throw = TR . Control.Exception.throw

throwt  :: Exception e => e -> T '[] '[Throws e] a
throwt= TR . Tr.throwt

-- * Utilities
genId :: T '[] '[] Int
genId= TR Tr.genId
