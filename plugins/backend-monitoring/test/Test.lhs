
\subsubsection{Test main entry point}

\begin{code}
{-# LANGUAGE CPP #-}

module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Bcc.BM.Test.Monitoring (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tbco-monitoring"
  [
    Bcc.BM.Test.Monitoring.tests
  ]
\end{code}

\subsubsection{Tests}

%include ../test/Bcc/BM/Test/Monitoring.lhs
