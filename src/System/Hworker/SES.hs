{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module System.Hworker.SES where

import           Control.Arrow           ((&&&))
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar)
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
                         , sesRecents :: MVar [UTCTime] }
data SESJob = SESJob { sesEmTo       :: Text
                     , sesEmSubj     :: Text
                     , sesEmBodyText :: Maybe Text
                     , sesEmBodyHtml :: Maybe Text
                     }
            deriving (Generic, Show)
instance ToJSON SESJob
instance FromJSON SESJob

instance Job SESState SESJob where
  job state@(SESState limit source recents) j@(SESJob to subj btxt bhtml) =
    do now <- getCurrentTime
       count <- length <$> modifyMVar recents
                                      (return .
                                       (id &&& id) .
                                       filter ((> 60) . diffUTCTime now))
       if count > limit
          then threadDelay 100000 >> job state j
          else do modifyMVar_ recents (return . (now:))
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
                    Right _ -> return Success

create :: Text -> Int -> Text -> IO (Hworker SESState SESJob)
create name limit source =
  do recents <- newMVar []
     Hworker.create name (SESState limit source recents)
