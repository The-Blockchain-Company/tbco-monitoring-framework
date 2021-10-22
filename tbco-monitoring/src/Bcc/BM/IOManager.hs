--
-- copied from https://github.com/The-Blockchain-Company/shardagnostic-network
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | A shim layer for `Win32-network`'s `IOManager`
--
module Bcc.BM.IOManager
  ( module X
  ) where

import           System.IOManager as X
