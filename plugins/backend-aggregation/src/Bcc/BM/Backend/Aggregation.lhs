
\subsection{Bcc.BM.Backend.Aggregation}
\label{code:Bcc.BM.Backend.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-@ embed GHC.Natural.Natural as int @-}

module Bcc.BM.Backend.Aggregation
    (
      Aggregation
    -- * Plugin
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           System.IO (stderr)

import           Bcc.BM.Backend.ProcessQueue (processQueue)
import           Bcc.BM.Configuration.Model (Configuration, getAggregatedKind)
import           Bcc.BM.Data.Aggregated (Aggregated (..), EWMA (..),
                     Measurable (..), ewma, singletonStats, updateAggregation)
import           Bcc.BM.Data.AggregatedKind (AggregatedKind (..))
import           Bcc.BM.Data.Backend
import           Bcc.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Bcc.BM.Data.LogItem
import           Bcc.BM.Data.Severity (Severity (..))
import           Bcc.BM.Data.Tracer (traceWith)
import           Bcc.BM.Plugin
import qualified Bcc.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Bcc.BM.Backend.Aggregation.Aggregation a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Internal representation}\label{code:Aggregation}\index{Aggregation}
\begin{code}
type AggregationMVar a = MVar (AggregationInternal a)
newtype Aggregation a = Aggregation
    { getAg :: AggregationMVar a }

data AggregationInternal a = AggregationInternal
    { agQueue    :: TBQ.TBQueue (Maybe (LogObject a))
    , agDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Relation from context name to aggregated statistics}
We keep the aggregated values (|Aggregated|) for a named context in a |HashMap|.
\begin{code}
type AggregationMap = HM.HashMap Text Aggregated
\end{code}

\subsubsection{|Aggregation| implements |effectuate|}\index{Aggregation!instance of IsEffectuator}

|Aggregation| is an |IsEffectuator|
Enter the log item into the |Aggregation| queue.
\begin{code}
instance IsEffectuator Aggregation a where
    effectuate agg item = do
        ag <- readMVar (getAg agg)
        nocapacity <- atomically $ TBQ.isFullTBQueue (agQueue ag)
        if nocapacity
        then handleOverflow agg
        else atomically $ TBQ.writeTBQueue (agQueue ag) $! Just item

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Aggregation's queue full, dropping log items!"
\end{code}

\subsubsection{|Aggregation| implements |Backend| functions}\index{Aggregation!instance of IsBackend}

|Aggregation| is an |IsBackend|
\begin{code}
instance FromJSON a => IsBackend Aggregation a where
    bekind _ = AggregationBK

    realize _ = fail "Aggregation cannot be instantiated by 'realize'"

    realizefrom config trace _ = do
        aggref <- newEmptyMVar
#ifdef PERFORMANCE_TEST_QUEUE
        let qSize = 1000000
#else
        let qSize = 2048
#endif
        aggregationQueue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher config HM.empty aggregationQueue trace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar aggref $ AggregationInternal aggregationQueue dispatcher
        return $ Aggregation aggref

    unrealize aggregation = do
        let clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getAg aggregation) (\ag ->
                            return (agDispatch ag, agQueue ag))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- wait for the dispatcher to exit
        -- TODO add a timeout to waitCatch in order
        -- to be sure that it will finish
        res <- Async.waitCatch dispatcher
        either throwM return res
        (clearMVar . getAg) aggregation

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: Configuration
                -> AggregationMap
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher conf aggMap aggregationQueue basetrace =
    let trace = Trace.appendName "#aggregation" basetrace
    in
    Async.async $ qProc trace aggMap
  where
    {-@ lazy qProc @-}
    qProc trace aggregatedMap =
        processQueue
            aggregationQueue
            processAggregated
            (trace, aggregatedMap)
            (\_ -> pure ())

    processAggregated lo@(LogObject loname lm _) (trace, aggregatedMap) = do
        (updatedMap, aggregations) <- update lo aggregatedMap trace
        sendAggregated trace loname (severity lm) aggregations
        return (trace, updatedMap)

    createNupdate :: Text -> Measurable -> LOMeta -> AggregationMap -> IO (Either Text Aggregated)
    createNupdate name value lme agmap = do
        case HM.lookup name agmap of
            Nothing -> do
                -- if Aggregated does not exist; initialize it.
                aggregatedKind <- getAggregatedKind conf name
                case aggregatedKind of
                    StatsAK      -> return $ Right (singletonStats value)
                    EwmaAK aEWMA ->
                        return $ AggregatedEWMA <$> ewma (EmptyEWMA aEWMA) value
            Just a -> return $ updateAggregation value a (utc2ns $ tstamp lme)

    update :: LogObject a
           -> AggregationMap
           -> Trace.Trace IO a
           -> IO (AggregationMap, [(Text, Aggregated)])
    update (LogObject loname lme (LogValue iname value)) agmap trace = do
        let fullname = loname <> "." <> iname
        eitherAggregated <- createNupdate fullname value lme agmap
        case eitherAggregated of
            Right aggregated -> do
                sendAggregated trace fullname (severity lme) [(iname, aggregated)]
                let updatedMap = HM.alter (const $ Just $ aggregated) fullname agmap
                return (updatedMap, [])
            Left w -> do
                let trace' = Trace.appendName "update" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                return (agmap, [])

    update (LogObject loname lme (ObserveDiff counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (loname, "diff") agmap [] trace
    update (LogObject loname lme (ObserveOpen counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (loname, "open") agmap [] trace
    update (LogObject loname lme (ObserveClose counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (loname, "close") agmap [] trace

    update (LogObject loname lme (LogMessage _)) agmap trace = do
        let iname  = pack $ show (severity lme)
        let fullname = loname <> "." <> iname
        eitherAggregated <- createNupdate fullname (PureI 0) lme agmap
        case eitherAggregated of
            Right aggregated -> do
                sendAggregated trace fullname (severity lme) [(iname, aggregated)]
                let updatedMap = HM.alter (const $ Just $ aggregated) fullname agmap
                return (updatedMap, [])
            Left w -> do
                let trace' = Trace.appendName "update" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                return (agmap, [])

    -- everything else
    update _ agmap _ = return (agmap, [])

    updateCounters :: [Counter]
                   -> LOMeta
                   -> (LoggerName,LoggerName)
                   -> AggregationMap
                   -> [(Text, Aggregated)]
                   -> Trace.Trace IO a
                   -> IO (AggregationMap, [(Text, Aggregated)])
    updateCounters [] _ _ aggrMap aggs _ = return (aggrMap, aggs)
    updateCounters (counter : cs) lme (logname, msgname) aggrMap aggs trace = do
        let name = cName counter
            subname = msgname <> "." <> (nameCounter counter) <> "." <> name
            fullname = logname <> "." <> subname
            value = cValue counter
        eitherAggregated <- createNupdate fullname value lme aggrMap
        case eitherAggregated of
            Right aggregated -> do
                let namedAggregated = (subname, aggregated)
                    updatedMap = HM.alter (const $ Just $ aggregated) fullname aggrMap
                updateCounters cs lme (logname, msgname) updatedMap (namedAggregated : aggs) trace
            Left w -> do
                let trace' = Trace.appendName "updateCounters" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                updateCounters cs lme (logname, msgname) aggrMap aggs trace

    sendAggregated :: Trace.Trace IO a -> Text -> Severity -> [(Text, Aggregated)] -> IO ()
    sendAggregated _trace _loname _sev [] = pure ()
    sendAggregated trace loname sev v = do
        meta <- mkLOMeta sev Public
        traceWith trace (loname, LogObject mempty meta (AggregatedMessage v))

\end{code}
