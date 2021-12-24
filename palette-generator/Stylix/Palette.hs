{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Stylix.Palette ( ) where

import Ai.Evolutionary ( Species(..) )
import Data.Bifunctor ( second )
import Data.Colour ( LAB(lightness), deltaE )
import Data.Vector ( (!), (//) )
import qualified Data.Vector as V
import System.Random ( RandomGen, randomR )

primary :: V.Vector a -> V.Vector a
primary = V.take 8

accent :: V.Vector a -> V.Vector a
accent = V.drop 8

alternatingZip :: V.Vector a -> V.Vector a -> V.Vector a
alternatingZip = V.izipWith (\i a b -> if even i then a else b)

randomFromVector :: (RandomGen r) => r -> V.Vector a -> (a, r)
randomFromVector generator vector
  = let (index, generator') = randomR (0, V.length vector - 1) generator
     in (vector ! index, generator')

instance (Floating a, Real a) => Species (V.Vector (LAB a)) (V.Vector (LAB a)) where
  generate image = generateColour 16
      where generateColour 0 generator = (generator, V.empty)
            generateColour n generator
              = let (colour, generator') = randomFromVector generator image
                 in second (V.cons colour) $ generateColour (n - 1) generator'

  crossover _ generator a b = (generator, alternatingZip a b)

  mutate image generator palette
    = let (index, generator') = randomR (0, 15) generator
          (colour, generator'') = randomFromVector generator' image
       in (generator'', palette // [(index, colour)])

  fitness _ palette
    = realToFrac $ accentDifference - min lightScheme darkScheme
      where accentDifference = minimum $ do
              a <- accent palette
              b <- accent palette
              return $ deltaE a b
            lightnesses = V.map lightness palette
            difference a b = abs $ a - b
            lightnessError primaryScale accentValue
              = sum (V.zipWith difference primaryScale $ primary lightnesses)
              + sum (V.map (difference accentValue) $ accent lightnesses)
            lightScheme
              = lightnessError (V.fromList [90, 70, 55, 35, 25, 10, 5, 5]) 40
            darkScheme
              = lightnessError (V.fromList [10, 30, 45, 65, 75, 90, 95, 95]) 60
