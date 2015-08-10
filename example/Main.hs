{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (mapM_)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           System.Hworker.SES

main :: IO ()
main = do hw <- create "ses-example"
                       2
                       "Daniel Patterson <dbp@dbpmail.net>"
                       (\e -> print e >> error "BLAH")
          forkIO (worker hw)
          forkIO (worker hw)
          forkIO (worker hw)
          forkIO (worker hw)
          forkIO (monitor hw)
          mapM_ (\n -> queue hw (SESJob "dbp@positiondev.com"
                                        ("Message #" <> T.pack (show n) <> "")
                                        (Just "Hi Daniel, this is the message in text.\n\n")
                                        (Just "<h2>Hi Daniel, this is the message in html.</h2>")
                                        (Right ("hello" :: T.Text, n :: Int))))
                [1..3]
          putStrLn "Waiting 10 seconds..."
          threadDelay 10000000
