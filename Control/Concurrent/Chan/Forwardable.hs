{- | A concurrent FIFO queue which behaves like Base's Chan with the new added 'forward' primative
 for merging two channels. Unlike duplicate style merging, when two channels are forwarded to each other,
 writes to one channel will not be duplicated to both channels, rather it will be as if both channels contents
 had been merged and all their outstanding and future references now point to this new channel.

 See <http://github.com/mmirman/forward-chan> for futher specification.
 -}
module Control.Concurrent.Chan.Forwardable ( Chan()
                                           , newChan
                                           , writeChan
                                           , readChan
                                           , forwardChan
                                           , getChanContents
                                           , writeList2Chan
                                           ) where

import Control.Concurrent.Chan.Forwardable.Internals

import qualified Control.Concurrent.Chan.Unagi as U
import Data.IORef
import Data.Typeable
import Control.Concurrent (forkIO)
import GHC.Base (join)
import System.IO.Unsafe ( unsafeInterleaveIO )

-- | Create a new channel
newChan :: IO (Chan a)
newChan = do
  (ci,co) <- U.newChan
  mi <- newIORef ci
  to <- newIORef co
  return $ Chan mi to

{- | Write a value to the channel.

If the channel becomes forwarded
at any time in the future, any thread
waiting on a value from the forwarded
channel may receive this value.
-}
writeChan (Chan mi _) v = do
  ci <- readIORef mi
  U.writeChan ci $ return v

{- | Read a value from a channel.
   
If a thread is waiting at a readChan
and another thread forwards this channel
to another (or visa versa) and the other
channel contains elements, this read might
now consume that element.
-}
readChan (Chan mi to) = join $ readIORef to >>= U.readChan

{- | Forward takes two channels and makes them act as one.
It obeys a few properties:
 
* /Commutivity/: @'forwardChan' a b === 'forwardChan' b a@

* /Behavioral Transitivity/: @('forwardChan' a b >> 'forwardChan' b c) === ('forwardChan' a b >> 'forwardChan' a c)@

* /Equal Opportunity/: For each of the following senarios (with @c1@ and @c2@ just created with 'newChan'), there are
possible executions which will print out "1" and possible executions which it will print "2", but it will never print both, and provided
one of the threads aren't starved by other thread, it will always print one of them.
it deadlock on both.

@
'writeChan' () c1
'forwardChan' c1 c2
'forkIO' $ do
   'readChan' c1
   putStrLn "1"
'readChan' c2
putStrLn "2"
@

@
'forwardChan' c1 c2
'writeChan' () c1
'forkIO' $ do
   'readChan' c1
   putStrLn "1"
'readChan' c2
putStrLn "2"
@

@
'forkIO' $ do
   'readChan' c1
   putStrLn "1"
'forkIO' $ do
    'readChan' c2
    putStrLn "2"
'forwardChan' c1 c2
'writeChan' () c1
@

@
'forkIO' $ do
   'readChan' c1
   putStrLn "1"
'forkIO' $ do
    'readChan' c2
    putStrLn "2"
'writeChan' () c1
'forwardChan' c1 c2
@

* /Early Bird Gets The Worm/: The first thread to read from either channel will, after a 'forward', always recieve
the next available item.  Similarly, items written to either channel are read in the same order they were written in.  The following examples will always print out "12"

@
'writeChan' "1" c1
'writeChan' "2" c2
'forwardChan' c1 c2
'readChan' c1 >>= putStr
'readChan' c2 >>= putStr
@

@
'writeChan' "1" c1
'writeChan' "2" c2
'forwardChan' c1 c2
'readChan' c1 >>= putStr
'readChan' c1 >>= putStr
@

@
'forwardChan' c1 c2
'writeChan' "1" c1
'writeChan' "2" c2
'readChan' c1 >>= putStr
'readChan' c2 >>= putStr
@

@
'forwardChan' c1 c2
'writeChan' "1" c1
'writeChan' "2" c2
'readChan' c2 >>= putStr
'readChan' c2 >>= putStr
@

* /Note/: if @a '==' b@ is @False@, then after @'forwardChan' a b@ will not cause @a '==' b@ to become @True@.

-}
forwardChan :: Chan a -> Chan a -> IO ()    
forwardChan c@(Chan mi to) (Chan mi' to') = do
  ci' <- readIORef mi
  atomicWriteIORef mi' =<< readIORef mi
  
  co' <- readIORef to'
  atomicWriteIORef to' =<< readIORef to
    
  let readIfAvailable av non = do
        (v,_) <- U.tryReadChan co'
        v <- U.tryRead v
        case v of
          Just _ -> av =<< join (U.readChan co')
          _ -> non

      getOldOrNew = readIfAvailable return $ readChan c

      useAll :: IO ()
      useAll = readIfAvailable (\v -> writeChan c v >> useAll) $ return ()
        
  useAll
  U.writeChan ci' $ getOldOrNew -- executes if we're stuck on a read.




-- | Return a lazy list representing the contents of the supplied 'Chan'
getChanContents :: Chan a -> IO [a]
getChanContents ch = unsafeInterleaveIO $ do
  x  <- readChan ch
  xs <- getChanContents ch
  return $ x:xs

-- | Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
