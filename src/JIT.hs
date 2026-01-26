-- | JIT Compilation Infrastructure
--
-- Shared state for JIT compilation, used by both Eval and VM.
module JIT
    ( -- * Call counting
      jitThreshold
    , incrementCallCount
      -- * Function IDs
    , newFuncId
      -- * Compiled code cache
    , getCompiledCode
    , cacheCompiledCode
    ) where

import Bytecode (CodeObject)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map

-- | Call count threshold for JIT compilation
jitThreshold :: Int
jitThreshold = 10

-- | Global call counter for functions (keyed by unique ID)
{-# NOINLINE callCounters #-}
callCounters :: IORef (Map.Map Int Int)
callCounters = unsafePerformIO $ newIORef Map.empty

-- | Global cache for compiled functions (keyed by unique ID)
{-# NOINLINE compiledCache #-}
compiledCache :: IORef (Map.Map Int CodeObject)
compiledCache = unsafePerformIO $ newIORef Map.empty

-- | Global counter for generating unique function IDs
{-# NOINLINE funcIdCounter #-}
funcIdCounter :: IORef Int
funcIdCounter = unsafePerformIO $ newIORef 0

-- | Get a new unique function ID
newFuncId :: IO Int
newFuncId = do
    n <- readIORef funcIdCounter
    writeIORef funcIdCounter (n + 1)
    return n

-- | Increment call count and return whether we should try to compile
incrementCallCount :: Int -> IO Bool
incrementCallCount fid = do
    counts <- readIORef callCounters
    let count = Map.findWithDefault 0 fid counts + 1
    writeIORef callCounters (Map.insert fid count counts)
    return (count >= jitThreshold)

-- | Check if function is already compiled
getCompiledCode :: Int -> IO (Maybe CodeObject)
getCompiledCode fid = do
    cache <- readIORef compiledCache
    return $ Map.lookup fid cache

-- | Cache compiled code for a function
cacheCompiledCode :: Int -> CodeObject -> IO ()
cacheCompiledCode fid code = do
    modifyIORef' compiledCache (Map.insert fid code)
