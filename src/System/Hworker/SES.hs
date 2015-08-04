{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module System.Hworker.SES where

import           Control.Arrow           ((&&&))
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Lens            (set)
import           Control.Monad.Trans.AWS hiding (page)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime)
import           GHC.Generics
import           Network.AWS.SES         hiding (Success)
import           System.Hworker          hiding (create)
import qualified System.Hworker          as Hworker (create)

data SESState = SESState { sesLimit   :: Int
                         , sesSource  :: Text
                         , sesRecents :: MVar [UTCTime]
                         , sesAfter   :: SESJob -> IO ()
                         }
data SESJob = SESJob { sesEmTo       :: Text
                     , sesEmSubj     :: Text
                     , sesEmBodyText :: Maybe Text
                     , sesEmBodyHtml :: Maybe Text
                     }
            deriving (Generic, Show)
instance ToJSON SESJob
instance FromJSON SESJob

instance Job SESState SESJob where
  job state@(SESState limit source recents after) j@(SESJob to subj btxt bhtml) =
    do now <- getCurrentTime
       rs <- takeMVar recents
       let active = filter ((< 1) . diffUTCTime now) rs
       let count = length active
       if count >= limit
          then putMVar recents active >> threadDelay 100000 >> job state j
          else do putMVar recents (now : active)
                  awsenv <- getEnv NorthVirginia Discover
                  r <- runAWST awsenv $
                       send (sendEmail source
                                       (set dToAddresses [to]
                                                         destination)
                                       (message (content subj)
                                                (set bHtml
                                                     (content <$> bhtml) $
                                                 set bText
                                                     (content <$> btxt)
                                                     body)))
                  case r of
                    Left err ->
                      do print err
                         return (Failure (T.pack (show err)))
                    Right _ -> do after j
                                  return Success

create :: Text -> Int -> Text -> (SESJob -> IO ()) -> IO (Hworker SESState SESJob)
create name limit source after =
  do recents <- newMVar []
     Hworker.create name (SESState limit source recents after)
