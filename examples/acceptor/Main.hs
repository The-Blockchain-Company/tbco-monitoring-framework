{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException, catch, throwIO)
import           Control.Monad (forever)
import           Data.Text (Text)

import           Bcc.BM.Backend.TraceAcceptor
import           Bcc.BM.Configuration
import qualified Bcc.BM.Configuration as Config
import           Bcc.BM.IOManager
import           Bcc.BM.Plugin (loadPlugin)
import qualified Bcc.BM.Setup as Setup
import           Bcc.BM.Trace
import qualified Bcc.BM.Tracing as Trace
import qualified Options.Applicative as Opt


data CLI
  = CLI
      { cConfig :: FilePath
      }

cliParser :: Opt.Parser CLI
cliParser = CLI
  <$> parseFilePath "config" "Config file"
 where
   parseFilePath :: String -> String -> Opt.Parser FilePath
   parseFilePath optname desc =
     Opt.strOption $ Opt.long optname <> Opt.metavar "FILEPATH" <> Opt.help desc

main :: IO ()
main = do
  cli <- Opt.customExecParser pref opts
  config <- readConfig (cConfig cli)
  acceptAt <- Config.getAcceptAt config
  case acceptAt of
    Just _ra -> do
      (tr :: Trace IO Text, sb) <- Setup.setupTrace_ config "bcc"
      let tr' = Trace.appendName "acceptor" tr
      withIOManager $ \iomgr -> do
        Bcc.BM.Backend.TraceAcceptor.plugin iomgr config tr' sb
          >>= loadPlugin sb
        forever $ threadDelay 1000000
    Nothing ->
      putStrLn $ "Trace acceptor not enabled in config: " <> cConfig cli
 where
   pref :: Opt.ParserPrefs
   pref = Opt.prefs Opt.showHelpOnEmpty

   opts :: Opt.ParserInfo CLI
   opts =
     Opt.info (cliParser Opt.<**> Opt.helper)
       ( Opt.fullDesc
         <> Opt.header
         "example-acceptor - utility to support a variety of key\
         \ operations (genesis generation, migration,\
         \ pretty-printing..) for different system generations."
       )

   readConfig :: FilePath -> IO Configuration
   readConfig fp =
     catch (Config.setup fp) $ \(e :: IOException) -> do
       putStrLn $ "Exception while reading configuration from: " <> fp
       throwIO e
