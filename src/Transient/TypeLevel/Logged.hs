{-# LANGUAGE CPP, DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}

module Transient.TypeLevel.Logged(
Lg.Loggable(..), logged, received, param, getLog, exec,wait, emptyLog,

#ifndef ghcjs_HOST_OS
 suspend, checkpoint, rerun, restore,
#endif

Lg.Log(..), Lg.toLazyByteString, Lg.byteString, Lg.lazyByteString, Lg.Raw(..)
) where

import Transient.TypeLevel.Effects
import qualified Transient.Logged as Lg
import Data.Typeable


logged :: Lg.Loggable a => T req eff a -> T req (Logged :> eff) a
logged (TR x)= TR $ Lg.logged x

received :: (Lg.Loggable a, Eq a) => a -> T '[] '[] ()
received= TR . Lg.received

param :: (Lg.Loggable a, Typeable a) => T '[] '[]  a
param= TR Lg.param

getLog ::  T '[] '[] Lg.Log
getLog= TR Lg.getLog

exec= Lg.exec
wait= Lg.wait

emptyLog= Lg.emptyLog

#ifndef ghcjs_HOST_OS
suspend :: Typeable a =>  a -> T '[] '[Logged] a
suspend = TR . Lg.suspend

checkpoint :: T '[] '[] ()
checkpoint = TR  Lg.checkpoint

rerun :: String ->T req eff a -> T req eff a
rerun s (TR x)= TR $ Lg.rerun s x

restore :: T req eff a -> T req eff a
restore (TR x) = TR $ Lg.restore x

#endif


