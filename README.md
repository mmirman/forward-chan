forward-chan 
============

* An more complete implementation of the forward primitive from the identity rule for the [proof terms for the sequent caluclus formulation of linear logic](http://www.cs.cmu.edu/~fp/papers/tldi12.pdf)

* The assurances of linear logic make the primitive stated in the paper above easy to implement - namely each channel can only be used once for input and once for output. Thus an implementation made entirely for linear logic need only read from one channel once and write the value to the forwarded channel.  In a non linear context however, this is a woefully insufficient primitive.  Ideally you'd want this to enable permanent (and retroactive) bidirectional forwarding, and not one time directional forwarding.  The remaining question is then how to build it and what specification it needs.   This package solves that problem.

* [on hackage](http://hackage.haskell.org/package/forward-chan)

Forward Specification
=====================

* _Commutivity_: `forwardChan a b === forwardChan b a`

* _Behavioral Transitivity_: `(forwardChan a b >> forwardChan b c) === (forwardChan a b >> forwardChan a c)`

* _Equal Opportunity_: For each of the following senarios (with `c1` and `c2` just created with `newChan`), there are
possible executions which will print out "1" and possible executions which it will print "2", but it will never print both, and provided
one of the threads aren't starved by other thread, it will always print one of them.
it deadlock on both.

```haskell
writeChan () c1
forwardChan c1 c2
forkIO $ do
   readChan c1
   putStrLn "1"
readChan c2
putStrLn "2"
```

```haskell
forwardChan c1 c2
writeChan () c1
forkIO $ do
   readChan c1
   putStrLn "1"
readChan c2
putStrLn "2"
```

```haskell
forkIO $ do
   readChan c1
   putStrLn "1"
forkIO $ do
    readChan c2
    putStrLn "2"
forwardChan c1 c2
writeChan () c1
```

```haskell
forkIO $ do
   readChan c1
   putStrLn "1"
forkIO $ do
    readChan c2
    putStrLn "2"
writeChan () c1
forwardChan c1 c2
```

* _Early Bird Gets The Worm_: The first thread to read from either channel will, after a `forward`, always recieve
the next available item.  Similarly, items written to either channel are read in the same order they were written in.  The following examples will always print out "12"

```haskell
writeChan "1" c1
writeChan "2" c2
forwardChan c1 c2
readChan c1 >>= putStr
readChan c2 >>= putStr
```

```haskell
writeChan "1" c1
writeChan "2" c2
forwardChan c1 c2
readChan c1 >>= putStr
readChan c1 >>= putStr
```

```haskell
forwardChan c1 c2
writeChan "1" c1
writeChan "2" c2
readChan c1 >>= putStr
readChan c2 >>= putStr
```

```haskell
forwardChan c1 c2
writeChan "1" c1
writeChan "2" c2
readChan c2 >>= putStr
readChan c2 >>= putStr
```
