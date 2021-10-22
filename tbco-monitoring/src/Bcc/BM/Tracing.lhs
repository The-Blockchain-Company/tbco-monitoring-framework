
\subsection{Bcc.BM.Tracing}
\label{code:Bcc.BM.Tracing}

%if style == newcode
\begin{code}

module Bcc.BM.Tracing
    ( Tracer (..)
    , Trace
    , LogObject (..)
    , PrivacyAnnotation (..)
    , Severity (..)
    , ToLogObject (..)
    , ToObject (..)
    , Transformable (..)
    , HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , TracingVerbosity (..)
    , appendName
    , contramap
    , defaultConfigStdout
    , defaultConfigTesting
    , mkLOMeta
    , nullTracer
    , setupTrace
    , traceWith
    ) where

import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import           Bcc.BM.Configuration.Static (defaultConfigStdout,
                     defaultConfigTesting)
import           Bcc.BM.Data.LogItem (LogObject (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Bcc.BM.Data.Severity (Severity (..))
import           Bcc.BM.Data.Trace (Trace)
import           Bcc.BM.Data.Tracer (HasPrivacyAnnotation (..),
                     HasSeverityAnnotation (..), ToLogObject (..),
                     ToObject (..), TracingVerbosity (..), Transformable (..))
import           Bcc.BM.Setup (setupTrace)
import           Bcc.BM.Trace (appendName)

\end{code}
%endif
