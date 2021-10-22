
\subsection{Bcc.BM.Counters.Dummy}
\label{code:Bcc.BM.Counters.Dummy}

This is a dummy definition of |readCounters| on platforms that do not support the
'proc' filesystem from which we would read the counters.

The only supported measurements are monotonic clock time and RTS statistics for now.

%if style == newcode
\begin{code}
{-# LANGUAGE CPP               #-}
module Bcc.BM.Counters.Dummy
    ( readCounters
    , readResourceStats
    ) where

#ifdef ENABLE_OBSERVABLES
import           Bcc.BM.Counters.Common (getMonoClock, readRTSStats)
import           Bcc.BM.Data.Observable
import           Bcc.BM.Stats.Resources
#endif
import           Bcc.BM.Data.Aggregated (Measurable(..))
import           Bcc.BM.Data.Counter
import           Bcc.BM.Data.SubTrace
\end{code}
%endif

\label{code:Dummy.readCounters}\index{Counters!Dummy!readCounters}
\begin{code}
readResourceStats :: IO (Maybe ResourceStats)
readResourceStats = pure . Just $ pure 0

readCounters :: SubTrace -> IO [Counter]
readCounters NoTrace                       = return []
readCounters Neutral                       = return []
readCounters (TeeTrace _)                  = return []
readCounters (FilterTrace _)               = return []
readCounters UntimedTrace                  = return []
readCounters DropOpening                   = return []
readCounters (SetSeverity _)               = return []
#ifdef ENABLE_OBSERVABLES
readCounters (ObservableTraceSelf tts)     = readCounters' tts []
readCounters (ObservableTrace     _   tts) = readCounters' tts []

readCounters' :: [ObservableInstance] -> [Counter] -> IO [Counter]
readCounters' [] acc = return acc
readCounters' (MonotonicClock : r) acc = getMonoClock >>= \xs -> readCounters' r $ acc ++ xs
readCounters' (GhcRtsStats    : r) acc = readRTSStats >>= \xs -> readCounters' r $ acc ++ xs
readCounters' (SysStats       : r) acc = readCounters' r $ acc ++ [Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum UnknownPlatform)]
readCounters' (_              : r) acc = readCounters' r acc
#else
readCounters (ObservableTraceSelf _)   = return [Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum UnknownPlatform)]
readCounters (ObservableTrace     _ _) = return []
#endif
\end{code}
