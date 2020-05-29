{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, PolyKinds,
  FlexibleContexts, UndecidableInstances, ConstraintKinds,
  ScopedTypeVariables, RebindableSyntax, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
  

module Transient.TypeLevel.Effects where



import Prelude  hiding (return, (>>=), (>>), (<*>),(<>))
import qualified Prelude as P (return, (>>=)) 
import qualified Control.Applicative as A

import qualified Transient.Internals as Tr
import qualified Transient.Indeterminism as In
import qualified Transient.Move as Cl


import qualified Control.Monad.Trans as T
import qualified Data.Monoid as M
import Control.Exception

data IOEff
data State a 
data RState a 


data EarlyTermination 
data Async -- may produce a response at a later time
data Streaming 
data MThread  -- Multi-threaded
data Cloud
data Throws e
data Handle e
data HasLog
data TerminalInput
data Logged
data Eff a     -- custom effect

-- Courtesy of Tim Pierson

type family (:>) x xs :: [*] where

    (:>) x '[] = '[x]

    (:>) x (x : xs) = (x : xs)

    (:>) y (x : xs) = ( x : (y :> xs))

type family (<:) x xs :: [*] where

    (<:)  '[] x = '[x]

    (<:) (x : xs) x = (x : xs)

    (<:) (x : xs) y = ( x : (y :> xs))




type family (:++) xs ys :: [*] where

    (:++)   '[] xs= xs

    (:++)  (x : xs) ys= (:++) xs ( x :> ys)

type family Member x xs :: Bool where

    Member x '[] = 'False

    Member x (x : xs) = 'True

    Member y (x : xs) = Member y xs


type family (:\) xs x :: [*] where
    (:\) '[] _= '[]
    
    (:\) (x : xs) x = xs

    (:\) (x : xs) y = x : (:\) xs y


type family Clean xs :: [*] where

    Clean xs = '[]








-----For alternative orElse composition

type family MNub x xs :: [*] where

    MNub x '[] = '[Maybe x]

    MNub (Maybe x) (Maybe x:xs)= (x : xs)

    MNub x (x : xs) = (x : xs)

    MNub x (Maybe x : xs) = (x : xs)

    MNub y (Maybe x : xs) = ( Maybe x : MNub y  xs)


    MNub y (x : xs) = ( Maybe x : MNub y  xs)





-- type family Alter xs ys :: [*] where

--     Alter  '[] xs= xs
--     Alter  (Async : xs) ys= (Async : (xs :++ ys))
--     Alter  (x : xs) ys=  Alter1 xs ( MNub x ys)



type family Alter xs ys :: [*] where

    Alter   '[] xs= xs
    Alter  (x : xs) ys= Alter xs (MNub x  ys)
---------------------------









newtype TR (eff:: [*]) m a= TR  (m a)                  


type T (eff:: [*])  a = TR (eff:: [*]) Tr.TransIO a 

type Pure a= T '[] a


instance T.MonadTrans (TR eff) where
    lift = TR

       --instance MonadIO m => MonadIO (TR eff m) where
liftIO  :: IO a -> T '[IOEff] a
liftIO = TR . T.liftIO

-- lift the Cloud monad
liftCl  :: Cl.Cloud a -> T effs a
liftCl = TR . Cl.runCloud'
 

instance Functor m => Functor (TR eff m) where
    fmap f (TR x)= TR $ fmap f x 

                                -- coerceEffs :: TR effs m a -> TR effs' m a
                                -- coerceEffs (TR comp)= TR comp

empty ::  T '[EarlyTermination] a
empty= TR A.empty

orElse ::  T (effs :: [*]) a -> T (effs' ::[*]) a -> T (Alter( effs  :\ EarlyTermination)  effs') a
TR a `orElse` TR b=  TR $  a A.<|>  b

(<|>) ::  T (effs :: [*]) a -> T (effs ::[*]) a -> T (( effs  :\ EarlyTermination) ) a
TR a <|> TR b=  TR $  a A.<|>  b

(<*>) ::  T (effs ::[*]) (a -> b) -> T (effs' ::[*]) a -> T (( effs :++ effs') :\  Async) b
TR a <*> TR b=  TR $  a A.<*>  b

mempty :: Monoid a => T '[] a
mempty = TR M.mempty

(<>) ::  Monoid a => T (effs ::[*]) a -> T (effs' ::[*]) a -> T ( effs :++  effs') a
TR a <> TR b=  TR $  a M.<>  b

return :: a -> T '[] a
return= TR. P.return 



(>>=) :: T (effs ::[*]) a -> (a -> T (effs' ::[*]) b) -> T  ( effs :++  effs')  b
(TR a) >>= b = TR $ a P.>>= \x -> let  TR z= b x in z  

--(>>) :: T (effs ::[*]) a -> T (effs' ::[*]) b -> T  ( effs :++ effs')  b
a >> b = a >>= \_ -> b
