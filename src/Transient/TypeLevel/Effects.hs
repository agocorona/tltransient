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
import qualified Control.Monad as M (guard)


data Terminates 
data Async -- may produce a response at a later time
data IOEff
data Streaming 
data Logged
data MThread  -- Multi-threaded
data Cloud
data Connected



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

    --MNub x ( Maybe Terminates :xs) = MNub x xs

    MNub (Maybe x) (Maybe x:xs)= (x : xs)

    MNub (Maybe x) (Maybe y:xs)= (Maybe x : MNub (Maybe y)  xs)

    MNub x (Maybe x : xs) = (x : xs)

    MNub y (Maybe x : xs) = ( Maybe x : MNub y  xs)

    MNub x (x : xs) = (x : xs)

    MNub x ( Async :xs) = (Async : MNub x xs)

    MNub y (x : xs) = ( Maybe x : MNub y  xs)


-- To treat alternatives
type family (:||) xs ys :: [*] where

    (:||)  '[] '[]= '[]

    (:||)  '[]  _= '[]

    (:||) (Terminates:_) xs=  FilterTerminate xs 

    (:||) xs (Terminates:ys)=  FilterTerminate xs

    (:||) (Maybe Terminates : xs) ys= (Alter1 xs ( MNub (Maybe Terminates) ys)) :\ Maybe (Maybe Terminates)

    (:||) (Async : xs) ys= Alter1 xs ( MNub Async ys)


    --(:||)  (Terminates : _) ys= ys

    (:||)  (x : xs) (x:ys)=   (x: (xs :|| ys))

    (:||)  (x : xs) ys    =   (x: (xs :|| ys))


-- to treat when the first term has a "Async"
type family Alter1 xs ys :: [*] where

    Alter1   '[] xs= xs

    Alter1  (x : xs) ys= Alter1 xs (MNub x  ys)



-- to filter any effect after "Terminates"
type family FilterTerminate xs ::[*] where
    FilterTerminate '[]= '[]
    FilterTerminate (Terminates:xs)= '[]
    FilterTerminate (x:xs)= x:FilterTerminate  xs

---------------------------









newtype TR (req:: [*]) (eff:: [*]) m a= TR  (m a)   deriving(Applicative, A.Alternative, Monad)

type T (req:: [*]) (eff:: [*])  a = TR (req:: [*]) (eff:: [*]) Tr.TransIO a


type Pure a= T '[]'[] a


instance T.MonadTrans (TR eff1 eff) where
    lift = TR

       --instance MonadIO m => MonadIO (TR eff m) where
liftIO  :: IO a -> T '[] '[IOEff] a
liftIO = TR . T.liftIO

-- lift the Cloud monad
liftCl  :: Cl.Cloud a -> T req effs a
liftCl = TR . Cl.runCloud'
 

instance Functor m => Functor (TR eff1 eff m) where
    fmap f (TR x)= TR $ fmap f x 

                                -- coerceEffs :: TR effs m a -> TR effs' m a
                                -- coerceEffs (TR comp)= TR comp

empty ::  T '[] '[Terminates] a
empty= TR A.empty


(<|>) :: T (req ::[*]) (effs :: [*]) a 
      -> T (req' ::[*]) (effs' ::[*]) a 
      -> T (req :++ req') ( effs  :|| effs' ) a
TR a <|> TR b=  TR $  a A.<|>  b

(<*>) :: T (req ::[*]) (effs ::[*]) (a -> b) 
      -> T (req' ::[*]) (effs' ::[*]) a 
      -> T (req :++ req') (( effs' :++ effs) :\  Async) b

TR a <*> TR b=  TR $  a A.<*>  b

mempty :: Monoid a => T '[] '[] a
mempty = TR M.mempty

(<>) ::  Monoid a => T (req ::[*])(effs ::[*]) a 
     -> T (req' ::[*]) (effs' ::[*]) a 
     -> T (req :++ req')( effs' :++  effs) a

TR a <> TR b=  TR $  a M.<>  b

return :: a -> T '[] '[] a
return= TR. P.return 

retEff ::a -> T '[] eff a
retEff= TR. P.return


(>>=) :: T (req ::[*]) (effs ::[*]) a -> (a -> T (req' ::[*])  (effs' ::[*]) b) 
      -> T ( (req :++  req') :\\ (effs' :++  effs)) ( effs' :++  effs)  b

(TR a) >>= b = TR $ a P.>>= \x -> let  TR z= b x in z  

(>>) :: T req effs a
          -> T req' effs' b
          -> T ((req :++ req') :\\ (effs' :++ effs)) (effs' :++ effs) b
a >> b = a >>= \_ -> b

guard  :: Bool -> T '[] '[Maybe Terminates] ()
guard= M.guard


