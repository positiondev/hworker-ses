{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module System.Hworker.SES ( SESWorker
                          , SESWorkerWith
                          , SESState
                          , SESJob(..)
                          , create
                          , queue
                          , worker
                          , monitor
    ) where

import           Control.Arrow           ((&&&))
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Lens            (set)
import           Control.Monad           (mzero)
import           Control.Monad.Trans.AWS hiding (page)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (Object), object, (.:), (.=))
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime)
import           GHC.Generics
import           Network.AWS.SES         hiding (Success)
import           System.Hworker          hiding (create)
import qualified System.Hworker          as Hworker (create)

type SESWorkerWith a = Hworker (SESState a) (SESJob a)
type SESWorker = SESWorkerWith ()

data SESState a = SESState { sesLimit   :: Int
                           , sesSource  :: Text
                           , sesRecents :: MVar [UTCTime]
                           , sesAfter   :: SESJob a -> IO ()
                           }

data SESJob a = SESJob { sesEmTo       :: Text
                       , sesEmSubj     :: Text
                       , sesEmBodyText :: Maybe Text
                       , sesEmBodyHtml :: Maybe Text
                       , sesPayload    :: a
                       }
              deriving (Generic, Show)
instance ToJSON a => ToJSON (SESJob a) where
  toJSON SESJob{..} = object [ "v" .= (1 :: Int)
                             , "to" .= sesEmTo
                             , "subj" .= sesEmSubj
                             , "text" .= sesEmBodyText
                             , "html" .= sesEmBodyHtml
                             , "payload" .= sesPayload
                             ]
instance FromJSON a => FromJSON (SESJob a) where
  parseJSON (Object v) = SESJob <$> v .: "to"
                                <*> v .: "subj"
                                <*> v .: "text"
                                <*> v .: "html"
                                <*> v .: "payload"
  parseJSON _ = mzero

instance (ToJSON a, FromJSON a, Show a) => Job (SESState a) (SESJob a) where
  job state@(SESState limit source recents after) j@(SESJob to subj btxt bhtml payload) =
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

create :: (ToJSON a, FromJSON a, Show a)
       => Text
       -> Int
       -> Text
       -> (SESJob a -> IO ())
       -> IO (SESWorkerWith a)
create name limit source after =
  do recents <- newMVar []
     Hworker.create name (SESState limit source recents after)
