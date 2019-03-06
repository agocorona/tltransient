# tltransient
Type-level transient.

An experiment to add detailed effect types for [transient](https://github.com/transient-haskell/transient) using @dorchard [type-level-sets](https://github.com/dorchard/type-level-sets) and a very simple graded monad.

Motivation:  
1) Stronger compile-time guarantees: some primitives, [like "collect"](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a65cf56ae53c159031bd123) do not work with infinite streams when there is only one thread available (threads 1).
2) I can get rid of the three-four-five monads (TransIO, Cloud, Widget... StateIO, and even IO!) and run everything in a single graded monad.
3) better type level control of state
4) Programmers can create their own type annotation for their applications, and specify invariants at compile time [example](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a5c8d6cba39a53f1a18df7b) 
5) Pretty readable signatures. This screen capture I posted [time ago](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a453b195355812e5728d765) of VS-Code with Intero show that the effects are determined statically and displayed when hovering over the computations. In this case, the effects run by `keep` are derived from the computations that it runs inside: ![image](https://files.gitter.im/Transient-Transient-Universe-HPlay/Lobby/Rtxy/image.png)

This is an example using the debugger:

```bash
>ghci Main.hs 
*Main> :t async :: IO a -> TRS '[Async, MThread] a

*Main> :t waitEvents

waitEvents :: IO a -> TRS '[Streaming, MThread] a

*Main> appasync= async (P.return "hello") <|> waitEvents (P.return "wold")

*Main> :t appasync

appasync :: TRS '[Async, MThread, Streaming] [Char]

*Main>
```

To do:
1) create modules for transient , universe and axiom 
2) enclose the Cloud monad and the widget monad to the same graded monad (TSR)
3) manage parameterized effects, like `State Int` instead of `StateInt` or `Threads 5` instead of `Multithreaded`
4) Handle spurious complex type errors when variables not in scope
