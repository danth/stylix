module Stylix.Output ( makeOutputTable ) where

import           Data.Colour (RGB (RGB))
import qualified Data.Vector as V
import           Data.Word   (Word8)
import           Text.JSON   (JSObject, toJSObject)
import           Text.Printf (printf)

toHexNum :: Double -> Word8
toHexNum = truncate

{- |
Convert a colour to a hexadecimal string.

>>> toHex (RGB 255 255 255)
"ffffff"
-}
toHex :: RGB -> String
toHex (RGB r g b) = printf "%02x%02x%02x" (toHexNum r) (toHexNum g) (toHexNum b)

-- | Convert a palette to the JSON format expected by Stylix's NixOS modules.
makeOutputTable :: V.Vector RGB -> JSObject String
makeOutputTable
  = toJSObject
  . V.toList
  . V.imap (\i c -> (printf "base%02X" i, c))
  . V.map toHex
