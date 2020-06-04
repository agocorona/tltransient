{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}

module Transient.TypeLevel.Indeterminism (
choose,  choose', chooseStream, collect, collect', group, groupByTime, burst
)where
import qualified Transient.Indeterminism as In
import qualified Transient.Base as Tr
import Transient.TypeLevel.Effects



-- | Converts a list of pure values into a transient task set. You can use the
-- 'threads' primitive to control the parallelism.
--
choose  ::  [a] -> T '[] [Async,MThread,Streaming]  a
choose= TR . In.choose

-- | transmit the end of stream
chooseStream  ::  [a] -> T '[] [Async,MThread]  (Tr.StreamData a)
chooseStream = TR . In.chooseStream

-- | Same as 'choose',  slower in some cases
--
choose' :: [a] ->  T '[] [Async,MThread,Streaming]  a
choose'= TR . In.choose'


-- | Collect the results of a task set in groups of @n@ elements.
--
group :: Int -> T req eff a -> T req eff [a]
group n (TR mx)= TR $ In.group n mx


-- | Collect the results of the first @n@ tasks.  Synchronizes concurrent tasks
-- to collect the results safely and kills all the non-free threads before
-- returning the results.  Results are returned in the thread where 'collect'
-- is called.
--
collect ::  Int -> T req eff a -> T '[MThread] (eff :\ MThread :\ Streaming) [a]
collect n (TR mx)= TR $ In.collect n mx 

-- | Like 'collect' but with a timeout. When the timeout is zero it behaves
-- exactly like 'collect'. If the timeout (second parameter) is non-zero,
-- collection stops after the timeout and the results collected till now are
-- returned.
--
collect' :: Int -> Int ->  T req eff a -> T '[MThread] (eff :\ MThread :\ Streaming) [a]
collect' n t (TR mx) = TR $ In.collect' n t mx


-- | insert `SDone` response every time there is a timeout since the last response

burst :: Int -> T req eff a -> T req eff (Tr.StreamData a)
burst timeout (TR mx)  =  TR $ In.burst timeout mx

-- | Collect the results of a task set, grouping all results received within
-- every time interval specified by the first parameter as `diffUTCTime`. 
groupByTime :: Monoid a => Int -> T req eff a -> T req eff a
groupByTime timeout (TR mx) = TR $ In.groupByTime timeout mx