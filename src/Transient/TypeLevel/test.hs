{-# LANGUAGE RebindableSyntax, TypeFamilies, TypeOperators,DataKinds,RankNTypes #-}
import  Prelude  hiding ((>>=),(>>),return)
import qualified Prelude as P
import Transient.TypeLevel.Effects
import Transient.TypeLevel.Base 
import Transient.TypeLevel.Move
import Control.Exception hiding (onException)


test1= do
        set "hello"
        set  (0:: Int)
        onException $ \(SomeException e) -> liftIO $ print e
        x <-  getState -- `orElse` return 0
        r <- async (P.return "hello")  `orElse` waitEvents (P.return "world")
        liftIO $ print (r,x :: Int)
        


-- >>> :t async
-- async :: IO a -> T '[Async, MThread] a
--

-- >>> :t waitEvents
-- waitEvents :: IO a -> T '[Streaming, MThread] a
--

test =  (liftIO (print "HELLO") >> async (P.return "hello"))  `orElse` waitEvents (P.return "world")

node= undefined

test3=  runAt node $ local $ return "hello"

-- >>> :t test3         
-- test3 :: T ((effs <: Logged) <: Cloud) a
--




-- >>> :t getState
-- getState
--   :: base-4.11.1.0:Data.Typeable.Internal.Typeable a =>
--      T '[EarlyTermination] a
--

-- >>> :t test
-- test :: T '[Maybe Streaming, MThread, Maybe Async] [Char]
--

--- >>> :t test
--- test
---   :: T '[EarlyTermination, IOEff, Streaming, MThread, Async,
---          Handle SomeException, State Int, State [Char]]
---        b
---



