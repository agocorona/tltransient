{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts  #-}
module Transient.TypeLevel.Base where

import qualified Transient.Base as Tr
import Transient.TypeLevel.Effects
import Data.Type.Set
import Data.Typeable
import Control.Exception

-- * Composition Operators
(**>) :: TRS effs a 
      -> TRS effs'  b 
      -> TRS  (Union effs  effs')   b
(**>) (TR x) (TR y) = TR $ x Tr.**> y



(<**) :: TRS effs a 
      -> TRS effs'  b 
      -> TRS  (Union effs  effs')   a
(<**) (TR x) (TR y) = TR $ x Tr.<** y

(<***) :: TRS effs a  
      -> TRS effs'  b 
      -> TRS  (Union effs  effs')   a
(<***) (TR x) (TR y) = TR $ x Tr.<*** y


-- * Running the monad
keep :: Typeable a => TRS (  effs) a -> IO (Maybe a)
keep (TR x) = Tr.keep x 

keep' :: Typeable a => TRS (  effs) a -> IO (Maybe a)
keep' (TR x) = Tr.keep' x 

stop = empty

exit :: Typeable a => a -> TRS '[] a
exit = TR . Tr.exit 

-- * Asynchronous console IO

option :: (Typeable b, Show b, Read b, Eq b)
       => b -> String  
       -> TRS '[TerminalInput]  b
option ret prompt= TR $ Tr.option ret prompt

input :: (Typeable a, Read a,Show a) 
      =>  (a -> Bool) 
      -> String 
      -> TRS '[TerminalInput]  a
input val prompt= TR  $ Tr.input val prompt

input' :: (Typeable a, Read a,Show a) => Maybe a -> (a -> Bool) -> String -> TRS '[TerminalInput]  a
input' ma val prompt= TR $ Tr.input' ma val prompt



-- * Task Creation


parallel :: IO (Tr.StreamData b) -> TRS '[MThread,Streaming] (Tr.StreamData b)
parallel= TR. Tr.parallel

async ::  IO a -> TRS '[Async,MThread]  a
async = TR . Tr.async 



waitEvents :: IO a -> TRS  '[Streaming,MThread]  a
waitEvents = TR . Tr.waitEvents

spawn :: IO a -> TRS  '[Streaming,MThread] a
spawn = TR . Tr.waitEvents

sample :: Eq a => IO a -> Int -> TRS '[Streaming,MThread] a
sample f n= TR $ Tr.sample f n

abduce :: TRS '[]  ()
abduce = TR Tr.abduce

-- * State management
set :: Typeable a => a -> TRS  '[State a]  ()
set = TR . Tr.setData

get ::  Typeable a => a -> TRS '[]  a
get x= getSData <|> Transient.TypeLevel.Effects.return x

extractST :: (Typeable s, Member (State s) effs ) => TRS  effs b -> TRS effs (s,b)
extractST (TR comp) = TR $ do
    x <- comp 
    s <- Tr.getSData
    Prelude.return (s,x)

setData :: Typeable a => a -> TRS  '[State a]  ()
setData= set

getSData ::  Typeable a => TRS '[EarlyTermination]  a
getSData = TR  Tr.getSData

getData :: Typeable a => TRS '[] (Maybe a)
getData = TR Tr.getData

delData  :: Typeable a => a -> TRS '[] ()
delData x= TR $ Tr.delData x

modifyData :: Typeable a => (Maybe a -> Maybe a) -> TRS '[] ()  --- problematic!!!!
modifyData = TR .Tr.modifyData

modifyData' :: Typeable a => (a -> a) ->  a -> TRS '[State a] a
modifyData' f x = TR $ Tr.modifyData' f x

try :: TRS effs a -> TRS effs a
try (TR mx)= TR $ Tr.try mx

setState :: Typeable a => a -> TRS  '[State a]  ()
setState= setData

getState ::  Typeable a => TRS '[EarlyTermination]  a
getState= getSData

delState :: Typeable a => a -> TRS '[] ()
delState= delData

modifyState :: Typeable a => (Maybe a -> Maybe a) -> TRS '[] ()
modifyState= modifyData

getRState :: Typeable a => TRS '[EarlyTermination]  a
getRState = TR Tr.getRState

setRState :: Typeable a => a -> TRS '[RState a] ()
setRState = TR . Tr.setRState



-- * Thread management

threads0 :: TRS effs  a -> TRS ( effs :\ MThread)  a
threads0 (TR x)= TR $ Tr.threads 0 x

threads :: Int -> TRS effs a -> TRS effs a
threads n (TR mx)= TR $ Tr.threads n mx

addThreads :: Int ->  TRS '[MThread] ()
addThreads = TR . Tr.addThreads

freeThreads :: TRS effs a -> TRS (effs :++'[MThread]) a
freeThreads (TR mx)= TR $ Tr.freeThreads mx

hookedThreads :: TRS effs a -> TRS effs a
hookedThreads (TR mx)= TR $ Tr.hookedThreads mx

oneThread :: TRS effs a -> TRS (effs :\MThread) a
oneThread (TR mx)= TR $ Tr.oneThread mx

killChilds :: TRS '[] ()
killChilds = TR  Tr.killChilds

-- * Exceptions
-- $exceptions

onException :: Exception e => (e -> TRS effs a) -> TRS (effs :++  '[ThrowException e]) ()
onException exc= TR $ Tr.onException $ \e ->  let TR r= exc e in r Prelude.>> Prelude.return ()

onException' :: Exception e 
             => TRS effs a 
             -> (e -> TRS effs' a) 
             -> TRS (effs :++ effs' :++ '[ThrowException e]) a
onException' (TR x) exc = TR $ x `Tr.onException'` \e -> let TR r= exc e in r

cutExceptions :: TRS '[] ()
cutExceptions= TR Tr.cutExceptions

continue :: TRS '[] ()
continue = TR $ Tr.continue

catcht :: Exception e => TRS effs b -> (e -> TRS effs' b) -> TRS (effs :++ effs' :++ '[ThrowException e]) b
catcht (TR x) (exc) = TR $ x `Tr.catcht` \e -> let TR r= exc e in  r 

throwt  :: Exception e => e -> TRS '[ThrowException e] a
throwt= TR . Tr.throwt

-- * Utilities
genId :: TRS '[] Int
genId= TR Tr.genId
