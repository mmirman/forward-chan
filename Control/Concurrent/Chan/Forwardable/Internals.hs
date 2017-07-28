module Control.Concurrent.Chan.Forwardable.Internals where

import qualified Control.Concurrent.Chan.Unagi as U
import Data.IORef
import Data.Typeable

-- | A forwardable channel.
-- It supports all the standard channel operations
-- as layed out in the base library (as of 4.10.0.0)
-- except 'dupChan'
data Chan a = Chan { inChan :: IORef (U.InChan (IO a)), -- ^ a reference to an in-channel of actions that output the value. Warning: custom playing with these internals is likely to break forward's nice properties.
                     outChan :: IORef (U.OutChan (IO a)) -- ^ a reference to an out-channel of actions that output the value. 
                   }
               deriving (Eq, Typeable)
