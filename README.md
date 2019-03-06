# tltransient
Type-level transient
An experiment to add detailed effect types for [transient](https://github.com/transient-haskell/transient) using @dorchard [type-level-sets](https://github.com/dorchard/type-level-sets) and a very simple graded monad.

Motivation:  
1) Stronger compile-time guarantees: some primitives, [like "collect"](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a65cf56ae53c159031bd123) do not work with infinite streams when there is only one thread available (threads 1).
2) I can get rid of the three monads and run everything in a single graded monad.
3) better type level control of state
4) Programmers can create their own type annotation for their applications, and specify invariants at compile time [example](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a5c8d6cba39a53f1a18df7b) 
5) Pretty readable signatures. This screen capture I posted [time ago](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=5a453b195355812e5728d765) of VS-Code with Intero show that the effects are determined statically and displayed when hovering over the computations: ![image](https://files.gitter.im/Transient-Transient-Universe-HPlay/Lobby/Rtxy/image.png)

To do:
1) create modules for transient , universe and axiom 
2) enclose the Cloud monad and the widget monad to the same graded monad (TSR)
3) manage parameterized states, like `State a`
