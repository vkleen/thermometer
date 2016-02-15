{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.Buspirate
  ( -- * General
    BuspirateM()
  , MonadBuspirate(..)
  , BuspirateError(..)
  , runBuspirate
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Typeable
import           Data.Word
import           System.IO
import           System.Hardware.Serialport


newtype BuspirateM a = BuspirateM { runBPMa :: ReaderT SerialPort IO a }
  deriving (Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader SerialPort, Functor, Applicative)

class (MonadIO m, MonadMask m, MonadReader SerialPort m) => MonadBuspirate m where
  command :: Word8 -> m ()
  commandData :: Word8 -> BS.ByteString -> m ()
  readData :: Int -> m BS.ByteString
  expect :: BS.ByteString -> m ()

data BuspirateError = InvalidResponse
  deriving (Show, Typeable)

instance Exception BuspirateError

instance MonadBuspirate BuspirateM where
  command c = do h <- ask
                 liftIO $ flush h
                 void $ liftIO $ send h (BS.singleton c)
                 expect "\x01"

  expect bs = do h <- ask
                 liftIO $ expect_ h bs

  commandData c bs = do command c
                        h <- ask
                        void $ liftIO $ send h bs

  readData n = do h <- ask
                  liftIO $ recv h n

expect_ h bs = do liftIO $ threadDelay 100000
                  r <- liftIO $ recv h (BS.length bs)
                  if r /= bs
                    then throwM InvalidResponse
                    else return ()

runBuspirate :: String -> BuspirateM a -> IO a
runBuspirate path (BuspirateM x) =
  withSerial path (defaultSerialSettings { commSpeed = CS115200 }) $ \h -> flip runReaderT h $
    bracket_ initBin resetBP x
  where initBin = do h <- ask
                     replicateM_ 10 $ do liftIO $ void $ send h "\n"
                                         liftIO $ threadDelay 10000
                     liftIO $ void $ send h "#\n"
                     replicateM_ 20 $ do liftIO $ void $ send h "\x00"
                                         liftIO $ threadDelay 10000
                     retry 10 $ do liftIO $ flush h
                                   liftIO $ void $ send h "\x00"
                                   expect_ h "BBIO1"
                     liftIO $ flush h
        resetBP = do h <- ask
                     liftIO $ void $ send h "\x00\x0f"
        retry 0     _ = throwM (InvalidResponse)
        retry times a = a `catch` (\(_ :: SomeException) -> retry (times-1) a)
