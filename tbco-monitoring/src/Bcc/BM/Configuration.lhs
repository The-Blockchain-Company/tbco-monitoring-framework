
\subsection{Bcc.BM.Configuration}
\label{code:Bcc.BM.Configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP #-}

module Bcc.BM.Configuration
    (
      CM.Configuration
    , CM.setup
    , CM.minSeverity
    , CM.setMinSeverity
    , CM.inspectSeverity
    , CM.setSeverity
    , CM.getAcceptAt
    , CM.getBackends
    , CM.getForwardTo
    , CM.setForwardTo
    , CM.getForwardDelay
    , CM.setForwardDelay
    , CM.getOption
    , CM.getMapOption
    , CM.getTextOption
    , CM.setOption
    , CM.setTextOption
    , CM.updateOption
    , CM.findSubTrace
    , CM.setSubTrace
    , CM.getEKGBindAddr
    , CM.getGraylogPort
    , CM.getPrometheusBindAddr
    , CM.getGUIport
    , CM.getMonitors
    , getTextOptionOrDefault
    , testSeverity
    , CM.evalFilters
    , CM.testSubTrace
    ) where

import           Data.Foldable (fold)
import           Data.Text (Text)
import           Data.Maybe (fromMaybe)

import qualified Bcc.BM.Configuration.Model as CM
import           Bcc.BM.Data.LogItem

\end{code}
%endif

see |Bcc.BM.Configuration.Model| for the implementation.

\label{code:getOptionOrDefault}\index{getOptionOrDefault}
\begin{code}
getTextOptionOrDefault :: CM.Configuration -> Text -> Text -> IO Text
getTextOptionOrDefault cg name def = fromMaybe def <$> CM.getTextOption cg name

\end{code}

\subsubsection{Test severities}\label{code:testSeverity}\index{testSeverity}
Test severity of the given |LOMeta| to be greater or equal to those of the specific |LoggerName|.

\begin{code}
testSeverity :: CM.Configuration -> LoggerName -> LOMeta -> IO Bool
testSeverity config loggername meta = do
    globminsev  <- CM.minSeverity config
    globnamesev <- CM.inspectSeverity config loggername
    let minsev = globminsev <> fold globnamesev
    return $ (severity meta) >= minsev

\end{code}
