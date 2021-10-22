
\subsection{Bcc.BM.Test.STM}
\label{code:Bcc.BM.Test.STM}

\begin{code}
module Bcc.BM.Test.STM (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Observing STM actions" [
      testProperty "minimal" prop_STM_observer
    ]


prop_STM_observer :: Bool
prop_STM_observer = True

\end{code}
