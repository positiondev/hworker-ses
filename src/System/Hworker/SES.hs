{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module System.Hworker.SES ( SESWorker
                          , SESWorkerWith
                          , SESState(..)
                          , SESJob(..)
                          , SESConfig(..)
                          , RedisConnection(..)
                          , defaultSESConfig
                          , create
                          , createWith
                          , queue
                          , worker
                          , monitor
                          , jobs
    ) where

import           Control.Applicative     ((<|>))
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception       (SomeException, catch)
import           Control.Lens            (set)
import           Control.Monad           (mzero, void)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (Object, String), object, (.:),
                                          (.=))
import qualified Data.HashMap.Strict     as HashMap
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime)
import           GHC.Generics
import           Network.AWS             (Credentials (Discover), newEnv,
                                          runAWS, runResourceT, send)
import           Network.AWS.SES         hiding (Success)
import           Network.AWS.Types       (Error)
import           System.Hworker          hiding (create, createWith)
import qualified System.Hworker          as Hworker (createWith)

type SESWorkerWith a = Hworker (SESState a) (SESJob a)
type SESWorker = SESWorkerWith ()

data SESState a = SESState { sesLimit   :: Int
                           , sesSource  :: Text
                           , sesRecents :: MVar [UTCTime]
                           , sesAfter   :: SESJob a -> IO ()
                           , sesLogger  :: forall b. Show b => b -> IO ()
                           }

data SESJob a = SESJob { sesEmTo       :: Text
                       , sesEmSubj     :: Text
                       , sesEmBodyText :: Maybe Text
                       , sesEmBodyHtml :: Maybe Text
                       , sesPayload    :: Either Value a
                       }
              deriving (Generic, Show)
instance ToJSON a => ToJSON (SESJob a) where
  toJSON SESJob{..} = object [ "v" .= (1 :: Int)
                             , "t" .= sesEmTo
                             , "s" .= sesEmSubj
                             , "x" .= sesEmBodyText
                             , "h" .= sesEmBodyHtml
                             , ("p", either id toJSON sesPayload)
                             ]
instance FromJSON a => FromJSON (SESJob a) where
  parseJSON (Object v) = SESJob <$> v .: "t"
                                <*> v .: "s"
                                <*> v .: "x"
                                <*> v .: "h"
                                <*> ((Right <$> (v .: "p")) <|>
                                     pure (Left (HashMap.lookupDefault (String "No 'p' field.") "p" v)))
  parseJSON _ = mzero

instance (ToJSON a, FromJSON a, Show a) => Job (SESState a) (SESJob a) where
  job state@(SESState limit source recents after log') j@(SESJob to' subj btxt bhtml _payload) =
    do now <- getCurrentTime
       rs <- takeMVar recents
       let active = filter ((< 1) . diffUTCTime now) rs
       let count = length active
       if count >= limit
          then putMVar recents active >> threadDelay 100000 >> job state j
          else do putMVar recents (now : active)
                  awsenv <- newEnv Discover
                  r <- catch (runResourceT $ runAWS awsenv $
                       do void $ send (sendEmail source
                                          (set dToAddresses [to']
                                                            destination)
                                          (message (content subj)
                                                   (set bHTML
                                                        (content <$> bhtml) $
                                                    set bText
                                                        (content <$> btxt)
                                                        body)))
                          return Success)
                          (\(err::Error) ->
                             do log' err
                                return (Failure (T.pack (show err))))
                  case r of
                    Success -> catch (after j)
                                     (\(e::SomeException) ->
                                       log' ("hworker-ses callback raised exception: " <> show e))
                    _ -> return ()
                  return r

data SESConfig a =
     SESConfig { sesconfigName             :: Text
               , sesconfigLimit            :: Int
               , sesconfigSource           :: Text
               , sesconfigAfter            :: SESJob a -> IO ()
               , sesconfigLogger           :: forall b. Show b => b -> IO ()
               , sesconfigFailedQueueSize  :: Int
               , sesconfigRedisConnectInfo :: RedisConnection
               }

defaultSESConfig :: (ToJSON a, FromJSON a, Show a)
                 => Text
                 -> Int
                 -> Text
                 -> (SESJob a -> IO ())
                 -> SESConfig a
defaultSESConfig name limit source after =
  let d = defaultHworkerConfig name ()
  in SESConfig name
               limit
               source
               after
               (hwconfigLogger d)
               (hwconfigFailedQueueSize d)
               (hwconfigRedisConnectInfo d)

create :: (ToJSON a, FromJSON a, Show a)
       => Text
       -> Int
       -> Text
       -> (SESJob a -> IO ())
       -> IO (SESWorkerWith a)
create name limit source after =
  createWith (defaultSESConfig name limit source after)

createWith :: (ToJSON a, FromJSON a, Show a)
           => SESConfig a
           -> IO (SESWorkerWith a)
createWith (SESConfig name limit source after logger failed' redis) =
  do recents <- newMVar []
     Hworker.createWith
               (defaultHworkerConfig name
                                     (SESState limit source recents after logger)) {
                                     hwconfigRedisConnectInfo = redis
                                    ,hwconfigFailedQueueSize = failed'
                                    ,hwconfigLogger = logger
               }
