{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Criterion.Config
import Criterion.Main
import Criterion.Monad
import Data.IORef
import System.IO.Unsafe

data Lock a b = Lock { lockMake :: IO (a b)
                     , lockPut  :: (a b) -> b -> IO ()
                     , lockTake :: (a b) -> IO b
                     }

lockMVar :: Lock MVar a
lockMVar = Lock newEmptyMVar putMVar takeMVar

lockTMVar :: Lock TMVar a
lockTMVar = Lock newEmptyTMVarIO
                 (\lock x -> atomically $ putTMVar lock x)
                 (atomically . takeTMVar)

forkJoin :: Lock a () -> IO ()
forkJoin Lock{..} = do
    lock <- lockMake
    _ <- forkIO (lockPut lock ())
    lockTake lock


cfg :: Config
cfg = defaultConfig
    -- Always GC between runs.
    { cfgPerformGC = ljust True
    , cfgTemplate  = ljust "report.tpl"
    , cfgReport    = ljust "report.html"
    }

prepare :: Criterion ()
prepare = liftIO $ do
    _ <- forkIO $ forever $ do
        () <- readChan theChan
        putMVar theLock ()
    
    _ <- forkIO $ forever $ do
        () <- atomically (readTChan theTChan)
        putMVar theLock ()
    
    _ <- forkIO $ forever $ do
        () <- takeMVar theMVar
        putMVar theLock ()
    
    _ <- forkIO $ forever $ do
        () <- atomically $ takeTMVar theTMVar
        putMVar theLock ()
    
    return ()

theChan :: Chan ()
theChan = unsafePerformIO newChan

theTChan :: TChan ()
theTChan = unsafePerformIO newTChanIO

theMVar :: MVar ()
theMVar = unsafePerformIO newEmptyMVar

theTMVar :: TMVar ()
theTMVar = unsafePerformIO newEmptyTMVarIO

theLock :: MVar ()
theLock = unsafePerformIO newEmptyMVar

theTMVarLock :: TMVar ()
theTMVarLock = unsafePerformIO newEmptyTMVarIO

theTVar :: TVar ()
theTVar = unsafePerformIO $ newTVarIO ()


theIORef :: IORef ()
theIORef = unsafePerformIO $ newIORef ()

main :: IO ()
main = defaultMainWith cfg prepare
    [ bgroup "write-read"
        [ bench "IORef" $ writeIORef theIORef () >> readIORef theIORef
        , bench "MVar" $ putMVar theLock () >> takeMVar theLock
        , bench "TVar" $
            atomically (writeTVar theTVar ()) >> atomically (readTVar theTVar)
        , bench "TMVar" $ do
            atomically $ putTMVar theTMVarLock ()
            atomically $ takeTMVar theTMVarLock
        ]
    , bgroup "forkIO-join"
        [ bench "MVar" $ forkJoin lockMVar
        , bench "TMVar" $ forkJoin lockTMVar
        ]
    , bgroup "send-join"
        [ bench "MVar" $ do
            putMVar theMVar ()
            takeMVar theLock
        , bench "Chan" $ do
            writeChan theChan ()
            takeMVar theLock
        , bench "TMVar" $ do
                atomically $ putTMVar theTMVar ()
                takeMVar theLock
        , bench "TChan" $ do
            atomically $ writeTChan theTChan ()
            takeMVar theLock            
        ]
    ]
