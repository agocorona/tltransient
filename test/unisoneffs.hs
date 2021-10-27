{-# LANGUAGE ExistentialQuantification #-}

-- https://www.slideshare.net/pjschwarz/game-of-life-polyglot-fp-haskell-scala-unison-part-3

data C a= Return a | forall b.Bind {sub:: C b,k :: b -> C a} | Suspend {resume:: IO a} 

instance Functor C
instance Applicative C
instance Monad C where
   return x= Return x
   x >>= f= Bind x f

run :: C a -> IO a
run (Return x)= return x
run (Suspend iox)= iox
run (Bind x f)=  case x of
   Return a ->  run $ f a
   Suspend r -> r >>= \x -> run $ f x
   Bind y g -> run (y >>= g >>= f) 

