
\subsection{Bcc.BM.Plugin}
\label{code:Bcc.BM.Plugin}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bcc.BM.Plugin
  ( Plugin (..)
  , loadPlugin
  )
  where

import           System.Remote.Monitoring (Server)

import           Bcc.BM.Backend.Log (Scribe)
import           Bcc.BM.Backend.Switchboard (Switchboard,
                     addExternalBackend, addExternalScribe, setSbEKGServer)
import           Bcc.BM.Data.Backend
import           Bcc.BM.Data.BackendKind ()
import           Bcc.BM.Data.Output

\end{code}
%endif

\subsubsection{Plugins extend functionality}\label{code:Plugin}\index{Plugin}
A |Plugin| has a name and is either a |Backend| or a |Scribe|.
\begin{code}

data Plugin a = BackendPlugin !(Backend a) BackendKind
              | ScribePlugin Scribe ScribeId
              | EKGPlugin !(Backend a) BackendKind (Maybe Server)
\end{code}

\subsubsection{Plugin behaviour}

\subsubsection{Integrating plugins}
\begin{code}
loadPlugin :: Switchboard a -> Plugin a -> IO ()
loadPlugin sb (BackendPlugin be bk) = do
    addExternalBackend sb be bk
loadPlugin sb (ScribePlugin sc nm) = do
  addExternalScribe sb sc nm
loadPlugin sb (EKGPlugin be bk condSv) = do
    setSbEKGServer condSv sb
    addExternalBackend sb be bk

\end{code}
