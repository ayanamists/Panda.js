{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main (main) where

import Data.Text (Text)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Text.Pandoc
import Text.Pandoc.App
  ( defaultOpts
  , Opt(..)
  , convertWithOpts)
import Text.Pandoc.Scripting (noEngine)
import Text.Pandoc.Filter (Filter(..))
import Data.Map as M (fromList)
import qualified Data.Text.IO as TIO
import Panda.Pandoc2JSX
import Panda.MetaData
import Options.Applicative

data PandaOpts = PandaOpts
  { inputFile :: Maybe String
  , meta :: Bool
  }

parsePandaOpts :: Parser PandaOpts
parsePandaOpts = PandaOpts
  <$> optional (strOption (long "inputFile"
     <> short 'i'
     <> metavar "PATH"))
  <*> switch ( long "meta"
            <> short 'm'
            <> help "whether output metadata")


runPandoc :: String -> PandaOpts -> IO ()
runPandoc input (PandaOpts _path _) = convertWithOpts noEngine opt'
      where opt' = defaultOpts { optInputFiles = (:[]) <$> _path
                               , optOutputFile = Just input
                               , optTo = Just "native"
                               , optFilters = [CiteprocFilter]
                               , optMetadata = Meta $ fromList [("link-citations", MetaBool True)]
                               , optStandalone = True
                               }


getResult :: PandocMonad m => PandaOpts -> Pandoc -> m Text
getResult (PandaOpts _ False) = writeJSX defaultJSXWriterOptions
getResult (PandaOpts _ True) = return . writeMeta


main :: IO ()
main = do
  let argParseOpts = info (parsePandaOpts <**> helper)
        (fullDesc <> progDesc "A Pandoc based m->jsx converter")

  pandaOpts <- execParser argParseOpts

  withSystemTempFile "temp.native" $ \tempFile tempHandle -> do
    hClose tempHandle
    runPandoc tempFile pandaOpts
    contents <- TIO.readFile tempFile
    result <- runIO $ readNative def contents >>= getResult pandaOpts
    case result of
      Left e -> print e
      Right _result -> TIO.putStrLn _result

