
\subsection{Bcc.BM.Data.Trace}
\label{code:Bcc.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.BM.Data.Trace
  ( Trace
  )
  where

import           Bcc.BM.Data.LogItem (LogObject(..), LoggerName)
import           Control.Tracer

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace m a| is a |Tracer| containing the context name and a |LogObject a|.
\begin{code}

type Trace m a = Tracer m (LoggerName, LogObject a)
\end{code}
