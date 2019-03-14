{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, PolyKinds,
  FlexibleContexts, UndecidableInstances, ConstraintKinds,
  ScopedTypeVariables, RebindableSyntax, GeneralizedNewtypeDeriving #-}

module Transient.TypeLevel.Effects where



import Prelude  hiding (return, (>>=), (>>))
import qualified Prelude as P (return, (>>=)) 
import qualified Control.Applicative as A

import qualified Transient.Base as Tr
import qualified Transient.Indeterminism as In
import qualified Transient.Move as Cl

import Data.Typeable
import qualified Control.Monad.Trans as T
import Data.Type.Set
import Data.Type.Bool 
import Data.Type.Equality
import qualified Data.Monoid as M


data IOEff
data State a 
data RState a 

data EarlyTermination 
data Async -- may produce a response at a later time
data Streaming 
data MThread  -- Multi-threaded
data Cloud
data ThrowException e
data HasLog
data TerminalInput
data Logged
data Eff a     -- custom effect

--data Mode = Streaming | MThread | Async

type family CmpModes a  b :: Ordering where
            CmpModes Async MThread              = 'LT
            CmpModes Async Streaming            = 'LT
            CmpModes Async IOEff                = 'LT
            
            

            CmpModes MThread Streaming          = 'LT
            CmpModes MThread (State a)          = 'LT

            CmpModes IOEff MThread              = 'LT
            CmpModes IOEff (State a)            = 'LT
            CmpModes Streaming (State a)        = 'LT

            CmpModes (State a) Async            = 'LT

            CmpModes (State a) (State b)        = 
                If (CmpModes b a == 'LT) 'GT (CmpModes b a)
            
            CmpModes (RState a) (State a)       = 'LT
            CmpModes (RState a) (RState b)        = 
                If (CmpModes b a == 'LT) 'GT (CmpModes b a)

            CmpModes (Eff a) (RState a)          = 'LT
            CmpModes (Eff a) (Eff b)        = 
                If (CmpModes b a == 'LT) 'GT (CmpModes b a)
                
            CmpModes (ThrowException e) (Eff a)          = 'LT
            CmpModes (ThrowException a) (ThrowException b)        = 
                If (CmpModes b a == 'LT) 'GT (CmpModes b a)           
            
            CmpModes a a                        = 'EQ
            CmpModes a b                        =
               If (CmpModes b a == 'LT) 'GT (CmpModes b a)


type instance Cmp a b = CmpModes a b

-- type family CmpNodes a b


newtype TR eff m a= TR  (m a)                   


type TRS effs  a = TR (Set effs) Tr.TransIO a 


instance T.MonadTrans (TR eff) where
    lift = TR

       --instance MonadIO m => MonadIO (TR eff m) where
liftIO  :: IO a -> TRS '[IOEff] a
liftIO = TR . T.liftIO

liftCl  :: Cl.Cloud a -> TRS effs a
liftCl = TR . Cl.runCloud'


instance Functor m => Functor (TR eff m) where
    fmap f (TR x)= TR $ fmap f x 

                                -- coerceEffs :: TR effs m a -> TR effs' m a
                                -- coerceEffs (TR comp)= TR comp

empty ::  TRS '[EarlyTermination] a
empty= TR A.empty

(<|>) ::  TRS effs a -> TRS effs' a -> TRS (Union (effs :\ EarlyTermination) effs') a
TR a <|> TR b=  TR $  a A.<|>  b


(<*>) ::  TRS effs (a -> b) -> TRS effs' a -> TRS (Union effs  effs' :\ Async) b
TR a <*> TR b=  TR $  a A.<*>  b

mempty :: Monoid a => TRS '[] a
mempty = TR M.mempty

(<>) ::  Monoid a => TRS effs a -> TRS effs' a -> TRS (Union effs  effs') a
TR a <> TR b=  TR $  a M.<>  b

return :: a -> TRS '[] a
return= TR. P.return 

(>>=) :: TRS effs a -> (a -> TRS effs' b) -> TRS  (Union effs  effs')  b
(TR a) >>= b = TR $ a P.>>= \x -> let  TR z= b x in z  

(>>) :: TRS effs a -> TRS effs' b -> TRS  (Union effs effs')  b
a >> b = a >>= \_ -> b
