{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Text.Pandoc
import Text.Pandoc.App (convertWithOpts,
                        defaultOpts, Opt(..), options,
                        parseOptionsFromArgs, handleOptInfo)
import Text.Pandoc.Scripting (noEngine)
import Text.Pandoc.Filter (Filter(..))
import Data.Map as M (fromList)
import System.Environment (getArgs, getProgName)
import qualified Data.Text.IO as TIO
import Panda.Pandoc2JSX

runPandoc :: FilePath -> [String] -> IO ()
runPandoc input args = do
  prg <- getProgName
  opt <- parseOptionsFromArgs options defaultOpts prg args
  case opt of
    Left info -> handleOptInfo noEngine info
    Right opt' -> convertWithOpts noEngine opt''
      where opt'' = opt' { optOutputFile = Just $ input
                         , optTo = Just "native"
                         , optFilters = [CiteprocFilter]
                         , optMetadata = Meta $ fromList [("link-citations", (MetaBool True))]
                         }


main :: IO ()
main = do
  rawArgs <- getArgs

  withSystemTempFile "temp.native" $ \tempFile tempHandle -> do
    hClose tempHandle
    runPandoc tempFile rawArgs
    contents <- TIO.readFile tempFile
    result <- runIO $ do
      doc <- readNative def contents
      writeJSX defaultJSXWriterOptions doc
    case result of
      Left e -> putStrLn $ show e
      Right _result -> TIO.putStrLn _result


