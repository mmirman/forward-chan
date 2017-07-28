forward-chan 
============

* An implementation of the forward primitive from the [proof terms for the sequent caluclus formulation of linear logic](http://www.cs.cmu.edu/~fp/papers/tldi12.pdf)

Forward Specification
=====================

* /Equal Opportunity/: For each of the following senarios (with @c1@ and @c2@ just created with 'newChan'), there are
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

* /Early Bird Gets The Worm/: The first thread to read from either channel will, after a 'forward', always recieve
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




