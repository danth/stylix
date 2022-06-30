{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Stylix.Palette ( ) where

import Ai.Evolutionary ( Species(..) )
import Data.Bifunctor ( second )
import Data.Colour ( LAB(lightness), deltaE )
import Data.Vector ( (!), (//) )
import qualified Data.Vector as V
import System.Random ( RandomGen, randomR )

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

-- | Select a random item from a vector.
randomFromVector :: (RandomGen r)
                 => r -- ^ Random generator
                 -> V.Vector a
                 -> (a, r) -- ^ Chosen item, new random generator
randomFromVector generator vector
  = let (index, generator') = randomR (0, V.length vector - 1) generator
     in (vector ! index, generator')

instance (Floating a, Real a) => Species (String, (V.Vector (LAB a))) (V.Vector (LAB a)) where
  {- |
  Palettes in the initial population are created by randomly
  sampling 16 colours from the source image.
  -}
  generate (_, image) = generateColour 16
      where generateColour 0 generator = (generator, V.empty)
            generateColour n generator
              = let (colour, generator') = randomFromVector generator image
                 in second (V.cons colour) $ generateColour (n - 1) generator'

  crossover _ generator a b = (generator, alternatingZip a b)

  {- |
  Mutation is done by replacing a random slot in the palette with
  a new colour, which is randomly sampled from the source image.
  -}
  mutate (_, image) generator palette
    = let (index, generator') = randomR (0, 15) generator
          (colour, generator'') = randomFromVector generator' image
       in (generator'', palette // [(index, colour)])

  fitness (polarity, _) palette
    = realToFrac $ accentDifference - scheme
      where
        -- The accent colours should be as different as possible.
        accentDifference = minimum $ do
          a <- accent palette
          b <- accent palette
          return $ deltaE a b

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
          = lightnessError (V.fromList [90, 70, 55, 35, 25, 10, 5, 5]) 40

        {-
        For dark themes, the background is dark and the text is bright.
        The accent colours are slightly brighter.
        -}
        darkScheme
          = lightnessError (V.fromList [10, 30, 45, 65, 75, 90, 95, 95]) 60
