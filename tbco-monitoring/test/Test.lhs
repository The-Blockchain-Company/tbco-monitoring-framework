
\section{Test main entry point}

\begin{code}
{-# LANGUAGE CPP #-}

module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Bcc.BM.Test.Aggregated (tests)
import qualified Bcc.BM.Test.STM (tests)
import qualified Bcc.BM.Test.Trace (tests)
import qualified Bcc.BM.Test.Configuration (tests)
import qualified Bcc.BM.Test.LogItem (tests)
import qualified Bcc.BM.Test.Rotator (tests)
import qualified Bcc.BM.Test.Routing (tests)
import qualified Bcc.BM.Test.Structured (tests)
import qualified Bcc.BM.Test.Tracer (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tbco-monitoring"
  [
    Bcc.BM.Test.Aggregated.tests
  , Bcc.BM.Test.STM.tests
  , Bcc.BM.Test.Trace.tests
  , Bcc.BM.Test.Configuration.tests
  , Bcc.BM.Test.LogItem.tests
  , Bcc.BM.Test.Rotator.tests
  , Bcc.BM.Test.Routing.tests
  , Bcc.BM.Test.Structured.tests
  , Bcc.BM.Test.Tracer.tests
  ]
\end{code}

\section{Test case generation}
%include ../test/Bcc/BM/Arbitrary/Aggregated.lhs

\section{Tests}

%include ../test/Bcc/BM/Test/LogItem.lhs
%include ../test/Bcc/BM/Test/Aggregated.lhs
%include ../test/Bcc/BM/Test/STM.lhs
%include ../test/Bcc/BM/Test/Trace.lhs
%include ../test/Bcc/BM/Test/Configuration.lhs
%include ../test/Bcc/BM/Test/Rotator.lhs
%include ../test/Bcc/BM/Test/Structured.lhs
%include ../test/Bcc/BM/Test/Tracer.lhs
