{-# LANGUAGE CPP #-}
module Bcc.BM.Stats
    ( Resources
    , ResourceStats
    , Platform.readResourceStats
    )
where

import           Bcc.BM.Stats.Resources

#if defined(linux_HOST_OS)
import qualified Bcc.BM.Counters.Linux as Platform
#elif defined(mingw32_HOST_OS)
import qualified Bcc.BM.Counters.Windows as Platform
#elif defined(darwin_HOST_OS)
import qualified Bcc.BM.Counters.Darwin as Platform
#else
import qualified Bcc.BM.Counters.Dummy as Platform
#endif
