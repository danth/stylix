{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stylix.Palette ( ) where

import           Ai.Evolutionary (Species (..))
import           Codec.Picture   (Image (imageHeight, imageWidth),
                                  PixelRGB8 (PixelRGB8), pixelAt)
import           Data.Colour     (LAB (lightness), RGB (RGB), deltaE, rgb2lab)
import           Data.List       (delete)
import           Data.Vector     ((//))
import qualified Data.Vector     as V
import           System.Random   (randomRIO)

-- | Extract the primary scale from a palette.
primary :: V.Vector a -> V.Vector a
primary = V.take 8

-- | Extract the accent colours from a palette.
accent :: V.Vector a -> V.Vector a
accent = V.drop 8

{- |
Combine two palettes by taking a colour from the left,
then the right, then the left, and so on until we have
taken enough colours for a new palette.
-}
alternatingZip :: V.Vector a -> V.Vector a -> V.Vector a
alternatingZip = V.izipWith (\i a b -> if even i then a else b)

randomFromImage :: Image PixelRGB8 -> IO LAB
randomFromImage image = do
  x <- randomRIO (0, imageWidth image - 1)
  y <- randomRIO (0, imageHeight image - 1)
  let (PixelRGB8 r g b) = pixelAt image x y
      color = RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
  return $ rgb2lab color

instance Species (String, Image PixelRGB8) (V.Vector LAB) where
  generate (_, image) = V.replicateM 16 $ randomFromImage image

  crossover _ a b = return $ alternatingZip a b

  mutate (_, image) palette = do
    index <- randomRIO (0, 15)
    colour <- randomFromImage image
    return $ palette // [(index, colour)]

  fitness (polarity, _) palette
    = realToFrac $ accentDifference - (primarySimilarity/10) - scheme
      where
        -- The primary scale should use similar colours, to an extent.
        primarySimilarity = maximum $ do
          a <- primary palette
          b <- primary palette
          return $ deltaE a b

        -- The accent colours should be as different as possible.
        accentDifference = minimum $ do
          index_x <- [0..7]
          index_y <- delete index_x [0..7]
          let x = accent palette V.! index_x
              y = accent palette V.! index_y
          return $ deltaE x y

        -- Helpers for the function below.
        lightnesses = V.map lightness palette
        difference a b = abs $ a - b

        lightnessError primaryScale accentValue
          -- The primary scale's lightnesses should match the given pattern.
          = sum (V.zipWith difference primaryScale $ primary lightnesses)
          -- The accent colours should all have the given lightness.
          + sum (V.map (difference accentValue) $ accent lightnesses)

        scheme = case polarity of
          "either" -> min lightScheme darkScheme
          "light"  -> lightScheme
          "dark"   -> darkScheme
          _        -> error ("Invalid polarity: " ++ polarity)

        {-
        For light themes, the background is bright and the text is dark.
        The accent colours are slightly darker.
        -}
        lightScheme
          = lightnessError (V.fromList [90, 70, 55, 35, 25, 10, 5, 5]) 40

        {-
        For dark themes, the background is dark and the text is bright.
        The accent colours are slightly brighter.
        -}
        darkScheme
          = lightnessError (V.fromList [10, 30, 45, 65, 75, 90, 95, 95]) 60
