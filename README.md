# trasient-typelevel
Type-level transient.

Contains a thin layer on top of the transient library with only compile-time redefintions of the primitives. It is designed to run the same transient code, after including the typelevel modules of this package and switching on the type-level extension of Haskell.

It adds detailed effect types for [transient](https://github.com/transient-haskell/transient-stack) using a very simple graded monad.

It will be a part of the transient stack. User can opt-in or opt-out to use it with the same code. At least this is the purpose.

It is by no means complete. There are now only redefinitions for the modules Transient.Base and Transient.Move.

Motivation:  
1) Stronger compile-time guarantees: Some primitives, [like "collect"](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a65cf56ae53c159031bd123) do not work with infinite streams when there is only one thread available (threads 1).
2) I can get rid of the three-four-five monads (TransIO, Cloud, Widget... StateIO, and even IO!) and run everything in a single graded monad.
3) better type level control of state
4) Programmers can create their own type annotation for their applications, and specify invariants at compile time(see example below)
5) Pretty readable signatures. This screen capture of VS-Code with the simple haskell extension show that the effects are determined statically and displayed when hovering over the computations. In this case, the effects run by `keep` are derived from the computations that it runs inside: 

![image](https://files.gitter.im/Transient-Transient-Universe-HPlay/Lobby/rrgg/image.png)


The types in typelevel-transient contains: 

```haskell
T (required ::[*])  (produced ::[*]) a
```
That is: T(ype), the required effects necessary for the computation and the effects produced by the computation.


The effects defined are

data EarlyTermination   -- The computation can stop
data Async              -- may produce a response at a later time
data IOEff              -- IO

data State a            -- State of type  `a`
data RState a           -- Mutable state, or reader (STRef)  of type `a`
data Streaming          -- Infinite stream
data MThread            -- Multi-threaded
data Throws e           -- Throws an exception of type  e
data Handle e           -- Handles an exception of type e
data TerminalInput      -- Uses the console
data Logged             -- The computation result has been logged
data Cloud              -- Perform remote computations

A computation can not compile if the required effects are not satisfied.

For example in the image above, test1 requires  `State String`. If I try to execute it with `keep'`

```haskell
*Main> keep' test1

<interactive>:1:7: error:
    • Couldn't match type ‘'[State String]’ with ‘'[]’
      Expected type: T '[]
                       '[State Int, IOEff, Handle SomeException, EarlyTermination, Async,
                         Maybe Streaming, MThread, Logged, Cloud]
                       ()
        Actual type: T '[State String]
                       '[State Int, IOEff, Handle SomeException, EarlyTermination, Async,
                         Maybe Streaming, MThread, Logged, Cloud]
                       ()
    • In the first argument of ‘keep'’, namely ‘test1’
      In the expression: keep' test1
      In an equation for ‘it’: it = keep' test1
```
However

```haskell
keep'$ set "hello" >> test1
```

does type check and executes (It produces a runtime error since atRemote neeed a connection stablished, so I  added a Connected effect)



## Custom effects ##

One really cool thing of this system , in my opinion, is the really low cost for creating custom effects in order to control invariants of the domain problem effectively.

Here I want to make sure that the stuff is not sent before the  payment has been processed:

```bash
data HasPaid
data SentStuff

pay  ::Int->  T '[]  '[HasPaid]()
pay i=  undefined

sendStuff ::   T '[HasPaid] '[SentStuff] ()
sendStuff =undefined


test5=   liftIO (print "hello") >> (return "hello" >> sendStuff)

test6=  pay 10 >> test5

*Main> :t test5
test5 :: T '[HasPaid] '[IOEff, SentStuff] ()    -- test5 need the HasPaid effect, produces IOEff, SendStuff

*Main> :t test6
test6 :: T '[] '[HasPaid, IOEff, SentStuff] ()  -- test6  need no effect (since pay is included)
```

 if pay is not present before sendStuff that would produce a compilation error

This has even more value in Transient, since the execution can span across different computers. It can support shutdowns and restarts, so sendStuff can execute in a remote computer some time after. Maintaining the paid flag in a single database and make programmers aware of that requirement are the kind of problems that can be eliminated.


