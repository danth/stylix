{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Stylix.Palette ( ) where

import Ai.Evolutionary ( Species(..) )
import Codec.Picture ( Image(imageWidth, imageHeight), PixelRGB8(PixelRGB8), pixelAt )
import Data.Colour ( LAB(lightness), RGB(RGB), deltaE, rgb2lab )
import Data.List ( delete )
import Data.Vector ( (//) )
import qualified Data.Vector as V
import System.Random ( randomRIO )

-- Adjust value according to coefficients
adjustValue :: Double -> Double->  Double -> Double -> Double
adjustValue v a b x = minimum [maximum [0.0, v*(a*x+b)], 100.0]

-- | Extract the primary scale from a pallete.
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

instance Species (String,Double,Double, Image PixelRGB8) (V.Vector LAB) where
  generate (_,_,_, image) = V.replicateM 16 $ randomFromImage image

  crossover _ a b = return $ alternatingZip a b

  mutate (_,_,_, image) palette = do
    index <- randomRIO (0, 15)
    colour <- randomFromImage image
    return $ palette // [(index, colour)]

  fitness (polarity,primaryScaleDark,primaryScaleLight, _) palette
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
          "light" -> lightScheme
          "dark" -> darkScheme
          _ -> error ("Invalid polarity: " ++ polarity)

        {-
        For light themes, the background is bright and the text is dark.
        The accent colours are slightly darker.
        -}
        lightScheme
          = lightnessError (V.fromList [
            (adjustValue 90.0 0.1 0.0 primaryScaleLight), 
            (adjustValue 70.0 0.963 0.037 primaryScaleLight),
            (adjustValue 55.0 0.913 0.087 primaryScaleLight),
            (adjustValue 35.0 0.167 0.883 primaryScaleLight),
            (adjustValue 25.0 0.078 0.922 primaryScaleLight),
            (adjustValue 10.0 0.056 0.944 primaryScaleLight),
            (adjustValue 5.0 0.0 1.0 primaryScaleLight),
            (adjustValue 5.0 0.0 1.0 primaryScaleLight)
          ]) 40

        -- 0.1, 0.133,0.178,0.85,0.93,0.95,1.0,1.0 for 0.1 scale
        -- f(x) = ax+b for multiplier (light)
        -- 1.0, 0.0
        -- 0.963, 0.037
        -- 0.913, 0.087
        -- 0.167, 0.833
        -- 0.078, 0.922
        -- 0.056, 0.944
        -- 0.0, 1.0
        -- 0.0, 1.0

        {-
        For dark themes, the background is dark and the text is bright.
        The accent colours are slightly brighter.
        -}
        darkScheme
          = lightnessError (V.fromList [
            (adjustValue 10.0 1.0 0.0 primaryScaleDark), 
            (adjustValue 30.0 0.963 0.037 primaryScaleDark),
            (adjustValue 45.0 0.913 0.087 primaryScaleDark),
            (adjustValue 65.0 0.167 0.883 primaryScaleDark),
            (adjustValue 75.0 0.078 0.922 primaryScaleDark),
            (adjustValue 90.0 0.056 0.944 primaryScaleDark),
            (adjustValue 95.0 0.0 1.0 primaryScaleDark),
            (adjustValue 95.0 0.0 1.0 primaryScaleDark)
            ]) 60
        
        -- 0.1, 0.133,0.178,0.85,0.93,0.95,1.0,1.0 for 0.1 scale
        -- f(x) = ax+b for multiplier (dark)
        -- 1.0, 0.0
        -- 0.963, 0.037
        -- 0.913, 0.087
        -- 0.167, 0.833
        -- 0.078, 0.922
        -- 0.056, 0.944
        -- 0.0, 1.0
        -- 0.0, 1.0
