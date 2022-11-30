{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import GHC.RTS.Events
import System.Environment
import Control.Monad
import System.IO


main :: IO ()
main = do
  putStrLn "Hello, Fast Haskell!"

  getArgs >>= \case
    [f] -> readEventLogFromFile f >>= \case
      Left err  -> putStrLn ("Couldn't parse eventlog: " ++ err)
      Right log -> parseEvents log
    _ -> putStrLn "Missing argument: eventlog file"

parseEvents :: EventLog -> IO ()
parseEvents (EventLog header (Data events)) = do
  putStrLn "The log"

  withFile "output.html" WriteMode \h -> do
    let strLn = hPutStrLn h
    strLn (show header)
    strLn "<ul>"
    forM_ events \ev -> do
      strLn $ "<li>" ++ show ev ++ "</li>"
    strLn "</ul>"
