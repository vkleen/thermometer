{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Concurrent
import qualified Control.Exception as C
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import qualified Options.Applicative as O
import           System.Exit
import           System.Hardware.Buspirate
import           System.Hardware.Buspirate.SPI
import           System.Hardware.MAX31855
import           System.Hardware.Serialport
import           System.IO
import           System.Posix.Signals

clearLine :: MonadIO m => m ()
clearLine = liftIO $ hPutStr stderr "\r\ESC[K"

printMAX31855 b = liftIO . hPutStr stderr . formatMAX31855 b

data Options = Options { path :: String
                       , update_interval :: Double
                       , show_internal_temp :: Bool }

runThermometer :: Options -> IO ()
runThermometer (Options path interval internal) =
  runBuspirate path $ withSPI $
    forever $ do d <- withCS $ transfer "\xff\xff\xff\xff"
                 clearLine
                 printMAX31855 internal $ decodeMAX31855 d
                 liftIO $ threadDelay (round $ interval * 10**6)

options :: O.Parser Options
options = Options <$> O.strArgument (   O.metavar "<path>"
                                     <> O.help "Path to the serial interface of the Bus Pirate")
                  <*> O.option O.auto (   O.long "update-interval"
                                       <> O.short 'u'
                                       <> O.metavar "<seconds>"
                                       <> O.help "Set the update interval in seconds"
                                       <> O.value 1
                                       <> O.showDefault)
                  <*> O.switch (   O.long "internal"
                                <> O.short 'i'
                                <> O.help "Show the internal temperature used for cold junction compensation")

main :: IO ()
main = do tid <- myThreadId
          installHandler keyboardSignal (Catch (throwTo tid C.UserInterrupt)) Nothing
          O.execParser opts >>= runThermometer
  where opts = O.info (O.helper <*> options)
                      (   O.fullDesc
                       <> O.progDesc "Simple thermometer using Bus Pirate and MAX31855"
                       <> O.header "thermometer - A Simple Thermometer")
