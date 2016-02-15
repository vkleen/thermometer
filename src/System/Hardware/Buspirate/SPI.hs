{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.Hardware.Buspirate.SPI
  ( -- * Types
    BuspirateSPI()
  , MonadBuspirateSPI(..)
  , withSPI
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.Typeable
import           System.Hardware.Buspirate
import           System.Hardware.Serialport

newtype BuspirateSPI a = BuspirateSPI (BuspirateM a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader SerialPort)

data SPISpeed
    = SPI30KHz
    | SPI125KHz
    | SPI250KHz
    | SPI1MHz
  deriving (Show, Ord, Eq, Enum, Bounded)

class (MonadIO m, MonadMask m) => MonadBuspirateSPI m where
  setSpeed :: SPISpeed -> m ()
  withCS :: m a -> m a
  transfer :: BS.ByteString -> m BS.ByteString

data BuspirateSPIError = DataTooLong
  deriving (Show, Typeable)

instance Exception BuspirateSPIError

instance MonadBuspirateSPI BuspirateSPI where
  setSpeed speed = BuspirateSPI $ command $ 0x60 + fromIntegral (fromEnum speed)

  withCS = bracket_ (BuspirateSPI $ command 0x02)
                    (BuspirateSPI $ command 0x03)

  transfer bs | BS.null bs || BS.length bs > 16 = throwM DataTooLong
              | otherwise = BuspirateSPI $ do commandData 0x13 bs
                                              liftIO $ threadDelay 100000
                                              readData $ BS.length bs

-- | Enter I2C mode and run given action
withSPI :: BuspirateSPI a -> BuspirateM a
withSPI (BuspirateSPI x)  = bracket_ (do h <- ask
                                         liftIO $ flush h
                                         liftIO $ void $ send h "\x01"
                                         expect "SPI1"
                                         command 0x49 -- HACK: set peripheral config statically
                                         command 0x8a -- HACK: set SPI config statically
                                         command 0x03 -- HACK: Deassert CS
                                     )
                                     (do h <- ask
                                         liftIO $ flush h
                                         liftIO $ void $ send h "\x00"
                                         expect "BBIO1")
                                     x
