module Control.Concurrent.Chan.Forwardable ( Chan()
                                           , newChan
                                           , writeChan
                                           , readChan
                                           , forwardChan
                                           ) where

import qualified Control.Concurrent.Chan.Unagi as U
import Data.IORef
import Control.Concurrent (forkIO)
import GHC.Base (join)

newtype Chan a = Chan { unChan :: (IORef (U.InChan (IO a)), IORef (U.OutChan (IO a))) }


newChan = do
  (ci,co) <- U.newChan
  mi <- newIORef ci
  to <- newIORef co
  return $ Chan (mi,to)



writeChan (Chan (mi,_)) v = do
  ci <- readIORef mi
  U.writeChan ci (return v)

readChan (Chan (mi,to)) = join $ readIORef to >>= U.readChan

  
forwardChan :: forall a . Chan a -> Chan a -> IO ()    
forwardChan c@(Chan (mi,to)) (Chan (mi',to')) = do
  ci' <- readIORef mi
  atomicWriteIORef mi' =<< readIORef mi
  
  co' <- readIORef to'
  atomicWriteIORef to' =<< readIORef to
    
  let readIfAvailable :: (a -> IO b) -> IO b -> IO b
      readIfAvailable av non = do
        (v,_) <- U.tryReadChan co'
        v <- U.tryRead v
        case v of
          Just _ -> av =<< join (U.readChan co')
          _ -> non

      getOldOrNew :: IO a
      getOldOrNew = readIfAvailable return (readChan c)

      useAll :: IO ()
      useAll = readIfAvailable (\v -> writeChan c v >> useAll) $ return ()
        
  useAll
  U.writeChan ci' $ getOldOrNew -- executes if we're stuck on a read.
    

void = (>> return ())

