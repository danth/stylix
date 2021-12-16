module Stylix.Output ( makeOutputTable ) where

import Data.Colour ( RGB(RGB) )
import qualified Data.Vector as V
import Data.Word ( Word8 )
import Text.JSON ( JSObject, toJSObject )
import Text.Printf ( printf )

makeOutputs :: (String, RGB Word8) -> [(String, String)]
makeOutputs (name, RGB r g b)
  = [ (name ++ "-dec-r", show $ fromIntegral r / 255)
    , (name ++ "-dec-g", show $ fromIntegral g / 255)
    , (name ++ "-dec-b", show $ fromIntegral b / 255)
    , (name ++ "-rgb-r", show r)
    , (name ++ "-rgb-g", show g)
    , (name ++ "-rgb-b", show b)
    , (name ++ "-hex-r", printf "%02x" r)
    , (name ++ "-hex-g", printf "%02x" g)
    , (name ++ "-hex-b", printf "%02x" b)
    , (name ++ "-hex", printf "%02x%02x%02x" r g b)
    , (name ++ "-hash", printf "#%02x%02x%02x" r g b)
    ]

toWord8 :: (RealFrac a) => RGB a -> RGB Word8
toWord8 (RGB r g b) = RGB (truncate r) (truncate g) (truncate b)

makeOutputTable :: (RealFrac a) => V.Vector (RGB a) -> JSObject String
makeOutputTable
  = toJSObject
  . concat
  . V.map makeOutputs
  . V.imap (\i c -> (printf "base%02X" i, c))
  . V.map toWord8
