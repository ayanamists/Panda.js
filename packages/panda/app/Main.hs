{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Text.Pandoc
import Text.Pandoc.App (convertWithOpts,
                        defaultOpts, Opt(..), options,
                        parseOptionsFromArgs, handleOptInfo)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Readers (readNative)
import Text.Pandoc.Writers.Native (writeNative)
import Text.Pandoc.Scripting (noEngine)
import System.Environment (getArgs, getProgName)
import qualified Data.Text.IO as TIO
import Lib

runPandoc :: FilePath -> [String] -> IO ()
runPandoc input args = do
  prg <- getProgName
  opt <- parseOptionsFromArgs options defaultOpts prg args
  case opt of
    Left info -> handleOptInfo noEngine info
    Right opt' -> convertWithOpts noEngine opt''
      where opt'' = opt' { optOutputFile = Just $ input  , optTo = Just "native" }


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
      Right result -> TIO.putStrLn result


