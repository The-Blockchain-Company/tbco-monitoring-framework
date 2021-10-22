
\subsection{Bcc.BM.Configuration.Static}
\label{code:Bcc.BM.Configuration.Static}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}

module Bcc.BM.Configuration.Static
    (
      defaultConfigStdout
    , defaultConfigTesting
    ) where

import qualified Bcc.BM.Configuration.Model as CM
import           Bcc.BM.Data.BackendKind
import           Bcc.BM.Data.Output
import           Bcc.BM.Data.Severity

\end{code}
%endif

\subsubsection{Default configuration outputting on |stdout|}
\begin{code}
defaultConfigStdout :: IO CM.Configuration
defaultConfigStdout = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK]
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "text"
                            , scFormat = ScText
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            }
                         ,  ScribeDefinition {
                              scName = "json"
                            , scFormat = ScJson
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            }
                         ]
    CM.setDefaultScribes c ["StdoutSK::text"]
    return c

\end{code}

\subsubsection{Default configuration for testing}
\begin{code}
defaultConfigTesting :: IO CM.Configuration
defaultConfigTesting = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK, AggregationBK]
    CM.setDefaultBackends c [KatipBK, AggregationBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "nooutput"
                            , scFormat = ScText
                            , scKind = DevNullSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            }
                      ]
    CM.setDefaultScribes c ["NullSK::nooutput"]

    return c

\end{code}
