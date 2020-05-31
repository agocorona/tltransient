{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, PolyKinds,
  FlexibleContexts, UndecidableInstances, ConstraintKinds,
  ScopedTypeVariables, RebindableSyntax, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
  

module Transient.TypeLevel.Effects where



import Prelude  hiding (return, (>>=), (>>), (<*>),(<>))
import qualified Prelude as P (return, (>>=)) 
import qualified Control.Applicative as A

import qualified Transient.Internals as Tr(TransIO)
import qualified Transient.Move as Cl


import qualified Control.Monad.Trans as T
import qualified Data.Monoid as M



data EarlyTermination 
data Async -- may produce a response at a later time
data IOEff
data Streaming 
data Logged


-- Courtesy of Tim Pierson:

type family (:>) x xs :: [*] where

    (:>) x '[] = '[x]

    (:>) x (Maybe x : xs)=  (x : xs)

    (:>) (Maybe x) (x : xs)= x : xs

    (:>) x (x : xs) = (x : xs)

    (:>) y (x : xs) = ( x : (y :> xs))


type family (<:) x xs :: [*] where

    (<:)  '[] x = '[x]

    (<:) (Maybe x : xs) x =  (x:xs)

    (<:) (x : xs) (Maybe x) =  (x:xs)

    (<:) (x : xs) x = (x : xs)

    (<:) (x : xs) y = ( x : (y :> xs))


type family (:++) xs ys :: [*] where

    (:++) '[] xs= xs
    
    (:++) (x : xs) ys= (:++) xs ( x :> ys)


type family (:\\) xs ys :: [*] where

    xs :\\ '[]= xs
    
    xs :\\ (y:ys)= (xs :\\ ys) :\y


type family Member x xs :: Bool where

    Member x '[] = 'False

    Member x (x : xs) = 'True

    Member y (x : xs) = Member y xs


type family And x y :: Bool  where
    And 'True 'True= 'True
    And  _  _ = 'False


type family Subset xs ys :: Bool where

    Subset '[] _  = 'True

    Subset xs '[] = 'False

    Subset (x:xs) ys= Member x ys `And` Subset xs ys


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

    MNub (Maybe x) (Maybe y:xs)= (Maybe x : MNub (Maybe y)  xs)

    MNub x (Maybe x : xs) = (x : xs)

    MNub y (Maybe x : xs) = ( Maybe x : MNub y  xs)

    MNub x (x : xs) = (x : xs)

    MNub x ( Async :xs) = (Async : MNub x xs)

    MNub y (x : xs) = ( Maybe x : MNub y  xs)



type family Alter xs ys :: [*] where

    Alter  '[] xs= xs

    Alter  (Async : xs) ys= Alter1 xs ( MNub Async ys)

    Alter  (x : xs) (x:ys)=   (x: (Alter xs  ys))

    Alter  (x : xs) ys=   (x: (Alter xs  ys))



type family Alter1 xs ys :: [*] where

    Alter1   '[] xs= xs
    Alter1  (x : xs) ys= Alter1 xs (MNub x  ys)
---------------------------









newtype TR (req:: [*]) (eff:: [*]) m a= TR  (m a)                  

type T (req:: [*]) (eff:: [*])  a = TR (req:: [*]) (eff:: [*]) Tr.TransIO a 


type Pure a= T '[]'[] a

type Tn e x= T '[] e x

instance T.MonadTrans (TR eff1 eff) where
    lift = TR

       --instance MonadIO m => MonadIO (TR eff m) where
liftIO  :: IO a -> Tn '[IOEff] a
liftIO = TR . T.liftIO

-- lift the Cloud monad
liftCl  :: Cl.Cloud a -> T req effs a
liftCl = TR . Cl.runCloud'
 

instance Functor m => Functor (TR eff1 eff m) where
    fmap f (TR x)= TR $ fmap f x 

                                -- coerceEffs :: TR effs m a -> TR effs' m a
                                -- coerceEffs (TR comp)= TR comp

empty ::  Tn '[EarlyTermination] a
empty= TR A.empty


(<|>) :: T (req ::[*]) (effs :: [*]) a 
      -> T (req' ::[*]) (effs' ::[*]) a 
      -> T (req :++ req') (Alter ( effs  :\ EarlyTermination) effs' ) a
TR a <|> TR b=  TR $  a A.<|>  b

(<*>) :: T (req ::[*]) (effs ::[*]) (a -> b) 
      -> T (req' ::[*]) (effs' ::[*]) a 
      -> T (req :++ req') (( effs' :++ effs) :\  Async) b

TR a <*> TR b=  TR $  a A.<*>  b

mempty :: Monoid a => Tn '[] a
mempty = TR M.mempty

(<>) ::  Monoid a => T (req ::[*])(effs ::[*]) a 
     -> T (req' ::[*]) (effs' ::[*]) a 
     -> T (req :++ req')( effs' :++  effs) a

TR a <> TR b=  TR $  a M.<>  b

return :: a -> Tn '[] a
return= TR. P.return 



(>>=) :: T (req ::[*]) (effs ::[*]) a -> (a -> T (req' ::[*])  (effs' ::[*]) b) 
      -> T ( (req :++  req') :\\ (effs' :++  effs)) ( effs' :++  effs)  b

(TR a) >>= b = TR $ a P.>>= \x -> let  TR z= b x in z  

(>>) :: T req effs a
          -> T req' effs' b
          -> T ((req :++ req') :\\ (effs' :++ effs)) (effs' :++ effs) b
a >> b = a >>= \_ -> b
