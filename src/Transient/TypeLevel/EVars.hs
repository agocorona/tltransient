{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}

module Transient.TypeLevel.EVars where
import Transient.TypeLevel.Effects
import qualified Transient.EVars as Ev

newEVar ::  T '[] '[] (Ev.EVar a)
newEVar = TR  Ev.newEVar

cleanEVar :: Ev.EVar a -> T '[] '[] ()
cleanEVar=TR . Ev.cleanEVar


readEVar :: Ev.EVar a -> T '[][Async,Streaming] a
readEVar= TR . Ev.readEVar

writeEVar :: Ev.EVar a -> a -> T '[] '[] ()
writeEVar ev x= TR $ Ev.writeEVar ev x

lastWriteEVar :: Ev.EVar a -> a -> T '[] '[] ()
lastWriteEVar ev x= TR $ Ev.lastWriteEVar ev x