{-# LANGUAGE MultiParamTypeClasses #-}

module Ai.Evolutionary ( EvolutionConfig(..), Species(..), evolve ) where

import Control.Applicative ( liftA2 )
import Data.Bifunctor ( second )
import Data.List ( mapAccumR, sortBy )
import Data.Ord ( Down(Down), comparing )
import System.Random ( RandomGen, randomR )

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct = liftA2 (,)

cartesianSquare :: [a] -> [(a, a)]
cartesianSquare as = as `cartesianProduct` as

repeatCall :: Int -> (a -> a) -> a -> a
repeatCall n f = (!! n) . iterate f

randomFromList :: (RandomGen r) => r -> [a] -> (a, r)
randomFromList generator list
  = let (index, generator') = randomR (0, length list - 1) generator
     in (list !! index, generator')

mapWithGen :: (r -> a -> (r, b)) -> (r, [a]) -> (r, [b])
mapWithGen = uncurry . mapAccumR

unfoldWithGen :: (r -> (r, a)) -> Int -> r -> (r, [a])
unfoldWithGen _ 0 generator = (generator, [])
unfoldWithGen f size generator =
  let (generator', as) = unfoldWithGen f (size - 1) generator
      (generator'', a) = f generator'
   in (generator'', a:as)

class Species environment genotype where
  generate :: (RandomGen r) => environment -> r -> (r, genotype)
  crossover :: (RandomGen r) => environment -> r -> genotype -> genotype -> (r, genotype)
  mutate :: (RandomGen r) => environment -> r -> genotype -> (r, genotype)
  fitness :: environment -> genotype -> Double

data EvolutionConfig = EvolutionConfig { populationSize :: Int
                                       , survivors :: Int
                                       , mutationProbability :: Double
                                       , generations :: Int
                                       }

randomMutation ::
  (RandomGen r, Species e g) =>
  e -> EvolutionConfig -> r -> g -> (r, g)
randomMutation environment config generator chromosome
  = let (r, generator') = randomR (0.0, 1.0) generator
     in if r <= mutationProbability config
        then mutate environment generator' chromosome
        else (generator', chromosome)

naturalSelection :: (Species e g) => e -> EvolutionConfig -> [g] -> [g]
naturalSelection environment config
  = map snd
  . take (survivors config)
  . sortBy (comparing fst)
  -- Avoid computing fitness multiple times during sorting
  -- Down reverses the sort order so that the best fitness comes first
  . map (\genotype -> (Down $ fitness environment genotype, genotype))

evolveGeneration ::
  (RandomGen r, Species e g) =>
  e -> EvolutionConfig -> (r, [g]) -> (r, [g])
evolveGeneration environment config (generator, population)
  = second (naturalSelection environment config)
  $ mapWithGen (randomMutation environment config)
  $ unfoldWithGen randomCrossover (populationSize config) generator
    where pairs = cartesianSquare population
          randomCrossover gen = let (pair, gen') = randomFromList gen pairs
                                 in (uncurry $ crossover environment gen') pair

initialGeneration ::
  (RandomGen r, Species e g) =>
  e -> EvolutionConfig -> r -> (r, [g])
initialGeneration environment config
  = unfoldWithGen (generate environment) (survivors config)

evolve :: (RandomGen r, Species e g) => e -> EvolutionConfig -> r -> (r, g)
evolve environment config generator
  = second head
  $ repeatCall (generations config) (evolveGeneration environment config)
  $ initialGeneration environment config generator
