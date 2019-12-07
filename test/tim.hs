{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, PolyKinds,
  FlexibleContexts, UndecidableInstances, ConstraintKinds,
  ScopedTypeVariables, RebindableSyntax, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
  
  
data T (a :: [*])

data F a

type family NubCons x xs :: [*] where

    NubCons x '[] = '[x]

    NubCons x (x : xs) = (x : xs)

    NubCons y (x : xs) = ( x : NubCons y xs)



type family ExistsIn x xs :: Bool where

    ExistsIn x '[] = 'False

    ExistsIn x (x : xs) = 'True

    ExistsIn y (x : xs) = ExistsIn y xs



tmp :: T '[Foo]

tmp = undefined



insrt :: F a -> T as -> T (NubCons a as)

insrt = undefined



fFoo :: F Foo

fFoo = undefined

fBar :: F Bar



fBar = undefined



test = insrt fBar $ insrt fFoo tmp



-- > :t test

-- > test :: T '[Foo, Bar]

