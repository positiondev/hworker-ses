{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           System.Hworker     hiding (create)
import           System.Hworker.SES

main :: IO ()
main = do hw <- create "ses-example" 5 "Daniel Patterson <dbp@dbpmail.net>"
          forkIO (worker hw)
          queue hw (SESJob "dbp@positiondev.com"
                           "This is a test from hworker-ses"
                           (Just "This is the message in text.\n\n")
                           (Just "<h2>This is the message in html.</h2>"))
          putStrLn "Waiting 10 seconds..."
          threadDelay 10000000
