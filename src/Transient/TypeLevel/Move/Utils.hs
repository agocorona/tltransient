{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}
module Transient.TypeLevel.Move.Utils(initNode,initNodeDef, initNodeServ, inputNodes, simpleWebApp, initWebApp
, onServer, onBrowser, atServer, atBrowser, runTestNodes, showURL,foldNet,exploreNet,exploreNetUntil)
 where

import Prelude
import qualified Transient.Base as Tr
import qualified Transient.Move.Internals as In
import qualified Transient.Move.Utils as Ut
import Transient.TypeLevel.Effects
import Data.Typeable

initNode :: (Tr.Loggable a,Member Cloud effs ~ True) => T req effs a -> T (req :\ Connected) (effs :\ Cloud) a
initNode (TR comp)= TR $ Ut.initNode (In.Cloud comp)

initNodeDef :: (Tr.Loggable a,Member Cloud effs ~ True) => String -> Int -> T (req :\ Connected)  effs a -> T req (effs :\ Cloud) a
initNodeDef host port (TR comp)= TR $ Ut.initNodeDef host port $ In.Cloud comp

initNodeServ :: (Tr.Loggable a,Member Cloud effs ~ True)  => In.Service -> String -> Int ->T (req :\ Connected)  effs a -> T req (effs :\ Cloud) a
initNodeServ service host port (TR comp)= TR $ Ut.initNodeServ service host port (In.Cloud comp)

inputNodes :: T '[] '[Cloud,Terminates] a
inputNodes = TR Ut.inputNodes

simpleWebApp :: (Typeable a, Tr.Loggable a,Member Cloud effs ~ True) => Integer -> T  req effs a -> IO ()
simpleWebApp port (TR proc)= TR $ Ut.simpleWebApp port  (In.Cloud proc)

initWebApp :: (Tr.Loggable a, Member Cloud effs ~ True)  => In.Node ->  T req effs a -> T req (effs :\ Cloud) a
initWebApp n (TR proc)= TR $ Ut.initWebApp port $ In.Cloud proc

onServer :: T req eff a -> T req (Maybe Terminates :> eff) a
onServer (TR proc)= TR $ Ut.onServer $ In.Cloud proc

onBrowser :: Member Cloud effs ~ True => T req effs a -> T req (Maybe Terminates :> effs) a
onBrowser(TR proc)= TR $ Ut.onBrowser $ In.Cloud proc

atServer :: (Tr.Loggable a, Member Cloud effs ~ True) => T req effs a -> T req  effs a
atServer(TR proc)= TR $ Ut.atServer $ In.Cloud proc

atBrowser :: (Tr.Loggable a, Member Cloud effs ~ True) => T req effs a -> T req  effs a
atBrowser(TR proc)= TR $ Ut.atBrowser $ In.Cloud proc

runTestNodes :: [Int] -> T '[]'[Cloud] ()
runTestNodes n=  TR $ In.runCloud' $ Ut.runTestNodes n

showURL :: T '[] '[Cloud]()
showURL= TR $ In.runCloud' Ut.showURL

-- foldNet :: (Loggable a, Member Cloud eff ~ True, Member Cloud eff' ~ True) 
--         => (T req eff a -> T req' eff' a -> T a) -> T req eff a -> T req' eff' a -> T (req :++ req') (eff :++ eff') a
-- foldNet =

-- exploreNet :: (Loggable a,Monoid a) => Cloud a -> Cloud a

-- exploreNetUntil ::  (Loggable a) => Cloud a -> Cloud  a
