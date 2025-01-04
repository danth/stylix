module Data.Colour ( LAB(..), RGB(..), deltaE, lab2rgb, rgb2lab ) where

data LAB = LAB { lightness :: Double
               , channelA  :: Double
               , channelB  :: Double
               }

data RGB = RGB { red   :: Double
               , green :: Double
               , blue  :: Double
               }

-- Based on https://github.com/antimatter15/rgb-lab/blob/master/color.js

deltaE :: LAB -> LAB -> Double
deltaE (LAB l1 a1 b1) (LAB l2 a2 b2) =
  let deltaL = l1 - l2
      deltaA = a1 - a2
      deltaB = b1 - b2
      c1 = sqrt $ a1^2 + b1^2
      c2 = sqrt $ a2^2 + b2^2
      deltaC = c1 - c2
      deltaH = deltaA^2 + deltaB^2 - deltaC^2
      deltaH' = if deltaH < 0 then 0 else sqrt deltaH
      sc = 1 + 0.045 * c1
      sh = 1 + 0.015 * c1
      deltaCkcsc = deltaC / sc
      deltaHkhsh = deltaH' / sh
      i = deltaL^2 + deltaCkcsc^2 + deltaHkhsh^2
   in if i < 0 then 0 else sqrt i

-- | Convert a 'LAB' colour to a 'RGB' colour
lab2rgb :: LAB -> RGB
lab2rgb (LAB l a bx) =
  let y = (l + 16) / 116
      x = a / 500 + y
      z = y - bx / 200
      x' = 0.95047 * (if x^3 > 0.008856 then x^3 else (x - 16/116) / 7.787)
      y' = if y^3 > 0.008856 then y^3 else (y - 16/116) / 7.787
      z' = 1.08883 * (if z^3 > 0.008856 then z^3 else (z - 16/116) / 7.787)
      r = x' * 3.2406 + y' * (-1.5372) + z' * (-0.4986)
      g = x' * (-0.9689) + y' * 1.8758 + z' * 0.0415
      b = x' *  0.0557 + y' * (-0.204) + z' *  1.0570
      r' = if r > 0.0031308 then 1.055 * r**(1/2.4) - 0.055 else 12.92 * r
      g' = if g > 0.0031308 then 1.055 * g**(1/2.4) - 0.055 else 12.92 * g
      b' = if b > 0.0031308 then 1.055 * b**(1/2.4) - 0.055 else 12.92 * b
   in RGB { red = max 0 (min 1 r') * 255
          , green = max 0 (min 1 g') * 255
          , blue = max 0 (min 1 b') * 255
          }

-- | Convert a 'RGB' colour to a 'LAB' colour
rgb2lab :: RGB -> LAB
rgb2lab (RGB r g b) =
  let r' = r / 255
      g' = g / 255
      b' = b / 255
      r'' = if r' > 0.04045 then ((r' + 0.055) / 1.055)**2.4 else r' / 12.92
      g'' = if g' > 0.04045 then ((g' + 0.055) / 1.055)**2.4 else g' / 12.92
      b'' = if b' > 0.04045 then ((b' + 0.055) / 1.055)**2.4 else b' / 12.92
      x = (r'' * 0.4124 + g'' * 0.3576 + b'' * 0.1805) / 0.95047
      y = r'' * 0.2126 + g'' * 0.7152 + b'' * 0.0722
      z = (r'' * 0.0193 + g'' * 0.1192 + b'' * 0.9505) / 1.08883
      x' = if x > 0.008856 then x**(1/3) else (7.787 * x) + 16/116
      y' = if y > 0.008856 then y**(1/3) else (7.787 * y) + 16/116
      z' = if z > 0.008856 then z**(1/3) else (7.787 * z) + 16/116
   in LAB { lightness = (116 * y') - 16
          , channelA = 500 * (x' - y')
          , channelB = 200 * (y' - z')
          }
