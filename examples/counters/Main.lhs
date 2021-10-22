\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever)

import           Bcc.BM.Configuration.Static (defaultConfigStdout)
import           Bcc.BM.Counters(readCounters)
import           Bcc.BM.Data.Counter
import           Bcc.BM.Data.LogItem
import           Bcc.BM.Data.Observable
import           Bcc.BM.Data.SubTrace (SubTrace(ObservableTraceSelf))
import           Bcc.BM.Data.Severity (Severity(Notice))
import           Bcc.BM.Setup (setupTrace_, shutdown)
import           Bcc.BM.Trace (Trace, appendName, logInfo, traceNamedObject)

\end{code}

\subsubsection{Entry procedure}
\begin{code}
main :: IO ()
main = do
    c <- defaultConfigStdout
    (tr :: Trace IO String, sb) <- setupTrace_ c "counters"

    let trace = appendName "node-metrics" tr
        counters = [MemoryStats, ProcessStats, NetStats, IOStats, GhcRtsStats, SysStats]
    thr <- Async.async $ forever $ do
        cts <- readCounters (ObservableTraceSelf counters)
        traceCounters trace cts
        logInfo trace "traced counters."
        threadDelay 5000000   -- 5 seconds

    Async.link thr
    threadDelay 30000000  -- 30 seconds
    Async.cancel thr
    shutdown sb
    return ()
  where
      traceCounters :: Trace IO String -> [Counter] -> IO ()
      traceCounters _tr [] = pure ()
      traceCounters tr (c@(Counter _ct cn cv) : cs) = do
        mle <- mkLOMeta Notice Confidential
        traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
        traceCounters tr cs

\end{code}
