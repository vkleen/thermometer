{-# LANGUAGE ScopedTypeVariables #-}
module System.Hardware.MAX31855 ( MAX31855(..)
                                , MAX31855Fault(..)
                                , decodeMAX31855
                                , formatMAX31855 ) where

import           Data.Binary.Strict.BitGet
import           Data.Bits
import           Data.Bool.Extras
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Word
import           Text.Printf

data MAX31855Fault = MAX31855Fault { regSCV :: Bool
                                   , regSCG :: Bool
                                   , regOC  :: Bool }
  deriving (Show)

data MAX31855 = MAX31855 { regTemp :: Double
                         , regIntTemp :: Double
                         , regFault :: Maybe MAX31855Fault }
  deriving (Show)

decodeMAX31855 :: BS.ByteString -> MAX31855
decodeMAX31855  = either error id <$> flip runBitGet decoder
  where decoder = do tc_temp <- getTemperature (-2) 14
                     skip 1
                     fault <- getBit
                     int_temp <- getTemperature (-4) 12
                     skip 1
                     failures <- getFailures fault
                     return $ MAX31855 tc_temp int_temp failures
        getTemperature scale bits = do raw_temp <- getAsInt16 bits
                                       return $ convert scale raw_temp
        convert scale = (* (2**scale)) . fromIntegral

        getAsInt16 :: Int -> BitGet Int16
        getAsInt16 bits = do raw <- getAsWord16 bits
                             return $ sign_extend raw
          where sign_extend :: Word16 -> Int16
                sign_extend x = if testBit x (bits-1)
                                   then fromIntegral $ x .|. ((2^16-1) `shiftL` bits)
                                   else fromIntegral x

        getFailures False = skip 3 *> return Nothing
        getFailures True = Just <$> (MAX31855Fault <$> getBit <*> getBit <*> getBit)

faultToErrMsg :: MAX31855Fault -> String
faultToErrMsg (MAX31855Fault scv scg oc) =
    unwords $ mconcat [ pure "FAULT"
                      , boolM scv "SCV"
                      , boolM scg "SCG"
                      , boolM oc  "OC" ]
  where boolM b x = bool mempty (pure x) b

tempStr :: Double -> Maybe Double -> String
tempStr temp (Just itemp) = printf "%+.2fC %+10.2fC" temp itemp
tempStr temp Nothing      = printf "%+.2fC" temp

formatMAX31855 :: Bool -> MAX31855 -> String
formatMAX31855 formatItemp (MAX31855 temp itemp fault) = maybe (tempStr temp (if formatItemp
                                                                                 then Just itemp
                                                                                 else Nothing))
                                                               faultToErrMsg fault
