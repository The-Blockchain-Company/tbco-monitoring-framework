\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import           Data.Aeson (FromJSON)

import           Bcc.BM.Backend.Switchboard (addUserDefinedBackend)
import           Bcc.BM.Data.Backend
import qualified Bcc.BM.Configuration.Model as CM
import           Bcc.BM.Configuration.Static (defaultConfigStdout)
#ifdef LINUX
import           Bcc.BM.Scribe.Systemd (plugin)
import           Bcc.BM.Data.Output (ScribeDefinition (..),
                     ScribePrivacy (..), ScribeKind (..), ScribeFormat (..))
import           Bcc.BM.Plugin (loadPlugin)
#endif
import           Bcc.BM.Setup (setupTrace_)
import           Bcc.BM.Trace (Trace, appendName, logDebug, logError,
                     logInfo, logNotice, logWarning)

\end{code}

\subsubsection{a simple backend}
\begin{code}
type MyBackendMVar a = MVar (MyBackendInternal a)
newtype MyBackend a = MyBackend { myBE :: MyBackendMVar a }
data MyBackendInternal a = MyBackendInternal {
                            counter :: Int
                         }

instance (FromJSON a) => IsBackend MyBackend a where
    bekind _ = UserDefinedBK "MyBackend"
    realize _ = MyBackend <$> newMVar (MyBackendInternal 0)
    unrealize be = putStrLn $ "unrealize " <> show (bekind be)

instance IsEffectuator MyBackend a where
    effectuate be _item = do
        modifyMVar_ (myBE be) $ \mybe ->
            return $ mybe { counter = counter mybe + 1}

    handleOverflow _ = putStrLn "Error: MyBackend's queue full!"

\end{code}

\subsubsection{Entry procedure}
\begin{code}
main :: IO ()
main = do
    c <- defaultConfigStdout
    CM.setDefaultBackends c [KatipBK, UserDefinedBK "MyBackend"]
#ifdef LINUX
    CM.setSetupBackends c [KatipBK, GraylogBK]
    CM.setDefaultBackends c [KatipBK, GraylogBK, UserDefinedBK "MyBackend"]
    CM.setGraylogPort c 3456
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "text"
                            , scFormat = ScText
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            , scRotation = Nothing
                            }
                         ,  ScribeDefinition {
                              scName = "json"
                            , scFormat = ScJson
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            , scRotation = Nothing
                            }
                         ]
    CM.setScribes c "simple.systemd" (Just ["JournalSK::bcc"])
#endif
    CM.setScribes c "simple.json" (Just ["StdoutSK::json"])
    (tr :: Trace IO String, sb) <- setupTrace_ c "simple"
    be :: MyBackend String <- realize c
    let mybe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
    addUserDefinedBackend sb mybe "MyBackend"
#ifdef LINUX
    -- inspect log with 'journalctl -t bcc'
    Bcc.BM.Scribe.Systemd.plugin c tr sb "bcc"
      >>= loadPlugin sb
#endif

    let trText = appendName "text" tr
        trJson = appendName "json" tr
#ifdef LINUX
        trSystemd = appendName "systemd" tr
#endif

    logDebug   trText    "this is a debug message\nwith a second line"
    logDebug   trJson    "this is a debug message\nwith a second line"
    logInfo    trText    "this is an information."
    logInfo    trJson    "this is an information."
    logNotice  trText    "this is a notice!"
    logNotice  trJson    "this is a notice!"
    logWarning trText    "this is a warning!"
    logWarning trJson    "this is a warning!"
    logError   trText    "this is an error!"
    logError   trJson    "this is an error!"
#ifdef LINUX
    logError   trSystemd "this is an error!"
#endif

    threadDelay 80000

    withMVar (myBE be) $ \backend ->
        putStrLn $ "read in total " ++ (show $ counter backend) ++ " messages."

    return ()

\end{code}
