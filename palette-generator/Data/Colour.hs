module Data.Colour ( RGB(..), HSV(..), rgbToHsv, hsvToRgb ) where

import Data.Fixed ( mod' )

-- http://mattlockyer.github.io/iat455/documents/rgb-hsv.pdf

data RGB a = RGB a a a deriving (Eq, Show) -- 0 to 255
data HSV a = HSV a a a deriving (Eq, Show) -- 0 to 1

normaliseHue :: (Real a) => a -> a
normaliseHue h = h `mod'` 6

rgbToHsv :: (Eq a, Fractional a, Num a, Real a) => RGB a -> HSV a
rgbToHsv (RGB r' g' b') = HSV h' s v
  where r = r' / 255
        g = g' / 255
        b = b' / 255
        maximal = maximum [r, g, b]
        minimal = minimum [r, g, b]
        delta = maximal - minimal
        h | delta == 0 = 0
          | maximal == r = (g - b) / delta
          | maximal == g = ((b - r) / delta) + 2
          | otherwise = ((r - g) / delta) + 4
        h' = normaliseHue h
        s | v == 0 = 0
          | otherwise = delta / v
        v = maximal

hsvToRgb :: (Num a, Ord a, Real a) => HSV a -> RGB a
hsvToRgb (HSV h' s v) = RGB r' g' b'
  where h = normaliseHue h'
        alpha = v * (1 - s)
        beta = v * (1 - (h - abs h) * s)
        gamma = v * (1 - (1 - (h - abs h)) * s)
        (r, g, b) | h < 1 = (v, gamma, alpha)
                  | h < 2 = (beta, v, alpha)
                  | h < 3 = (alpha, v, gamma)
                  | h < 4 = (alpha, beta, v)
                  | h < 5 = (gamma, alpha, v)
                  | otherwise = (v, alpha, beta)
        r' = r * 255
        g' = g * 255
        b' = b * 255
