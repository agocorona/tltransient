{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}
module Transient.TypeLevel.Move.Utils(initNode,initNodeDef, initNodeServ, inputNodes, simpleWebApp, initWebApp
, onServer, onBrowser, atServer, atBrowser, runTestNodes, showURL,foldNet,exploreNet,exploreNetUntil)
 where

import Prelude
import qualified Transient.Base as Tr
import qualified Transient.Move.Internals as In
import qualified Transient.Move.Utils as Ut
import Transient.TypeLevel.Effects


initNode :: (Tr.Loggable a,Member Cloud effs) => T req effs a -> T req (effs :\ Cloud) a
initNode (TR comp)= Ut.initNode comp

initNodeDef :: (Tr.Loggable a,Member Cloud effs) => String -> Int -> T req effs a -> T req (effs :\ Cloud) a
initNodeDef host port (TR comp)= Ut.initNodeDef host port comp

initNodeServ :: (Tr.Loggable a,Member Cloud effs)  => In.Service -> String -> Int ->T req effs a -> T req (effs :\ Cloud) a
initNodeServ service host port (TR comp)= TR $ Ut.initNodeServ service host port comp

inputNodes :: T '[] '[Cloud,Terminates] a
inputNodes = Ut.inputNodes

simpleWebApp :: (Typeable a, Loggable a,Member Cloud effs) => Integer -> T  req eff a -> IO ()
simpleWebApp port (TR proc)= TR $ Ut.simpleWebApp port proc

initWebApp :: (Loggable a, Member Cloud effs ~ True)  => In.Node ->  T req effs a -> T req (effs :\ Cloud) a
initWebApp n (TR proc)= TR $ Ut.initWebApp port proc

onServer :: T req eff a -> T req (Maybe Terminates :> eff) a
onServer (TR proc)= TR $ Ut.onServer proc

onBrowser :: Member Cloud effs ~ True => T req effs a -> T req (Maybe Terminates :> effs) a
onBrowser(TR proc)= TR $ Ut.onBrowser proc

atServer :: (Loggable a, Member Cloud effs ~ True) => T req effs a -> T req  effs a
atServer(TR proc)= TR $ Ut.atServer proc

atBrowser :: (Loggable a, Member Cloud effs ~ True) => T req effs a -> T req  effs a
atBrowser(TR proc)= TR $ Ut.atServer proc

runTestNodes :: [Int] -> T '[]'[Cloud] ()
runTestNodes=  TR . Ut.runTestNodes

showURL :: Cloud ()
showURL= TR Ut.showURL

foldNet :: Loggable a, Member Cloud eff ~ True, Member Cloud eff ~ True,Member Cloud eff ~ True) 
        => (T req eff a -> T req' eff' a -> T a) -> T req eff a -> T req' eff' a -> T (req :++ req') )(eff :++ eff') a
foldNet 

exploreNet :: (Loggable a,Monoid a) => Cloud a -> Cloud a

exploreNetUntil ::  (Loggable a) => Cloud a -> Cloud  a
