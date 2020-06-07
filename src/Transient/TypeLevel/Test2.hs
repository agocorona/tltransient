{-# LANGUAGE RebindableSyntax, TypeFamilies, TypeOperators,DataKinds,RankNTypes, ScopedTypeVariables #-}
import  Prelude  hiding ((>>=),(>>),return,concat)
import qualified Prelude as P 
import qualified Transient.TypeLevel.Effects as Eff
import Transient.TypeLevel.Base 
import Transient.TypeLevel.Move
import Transient.TypeLevel.Indeterminism
import Control.Exception hiding (onException)

(>>=)= (Eff.>>=)