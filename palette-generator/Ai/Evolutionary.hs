{-# LANGUAGE MultiParamTypeClasses #-}

module Ai.Evolutionary ( Species(..), evolve ) where

import           Data.Ord                     (Down (Down), comparing)
import           Data.Vector                  ((!))
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro (selectBy)
import           System.Random                (randomRIO)
import           Text.Printf                  (printf)

numSurvivors :: Int
numSurvivors = 500

numNewborns :: Int
numNewborns = 50000 - numSurvivors

mutationProbability :: Double
mutationProbability = 0.75

randomFromVector :: V.Vector a -> IO a
randomFromVector vector = do
  index <- randomRIO (0, V.length vector - 1)
  return $ vector ! index

{- |
A genotype is a value which is generated by the genetic algorithm.

The environment is used to specify the problem for which
we are trying to find the optimal genotype.
-}
class Species environment genotype where
  -- | Randomly generate a new genotype.
  generate :: environment -> IO genotype

  -- | Randomly mutate a single genotype.
  mutate :: environment -> genotype -> IO genotype

  -- | Randomly combine two genotypes.
  crossover :: environment -> genotype -> genotype -> IO genotype

  -- | Score a genotype. Higher numbers are better.
  fitness :: environment -> genotype -> Double

initialPopulation :: Species e g
                  => e -- ^ Environment
                  -> IO (V.Vector g) -- ^ Population
initialPopulation environment
  = V.replicateM numSurvivors (generate environment)

-- | Expand a population by crossovers followed by mutations.
evolvePopulation :: Species e g
                 => e -- ^ Environment
                 -> V.Vector g -- ^ Survivors from previous generation
                 -> IO (V.Vector g) -- ^ New population
evolvePopulation environment population = do
  let randomCrossover = do
        a <- randomFromVector population
        b <- randomFromVector population
        crossover environment a b

      randomMutation chromosome = do
        r <- randomRIO (0.0, 1.0)
        if r <= mutationProbability
          then mutate environment chromosome
          else return chromosome

  newborns <- V.replicateM numNewborns randomCrossover
  let nonElites = V.tail population V.++ newborns
  nonElites' <- V.mapM randomMutation nonElites
  return $ V.head population `V.cons` nonElites'

selectSurvivors :: Species e g
                => e -- ^ Environment
                -> V.Vector g -- ^ Original population
                -> (Double, V.Vector g) -- ^ Best fitness, survivors
selectSurvivors environment population =
  let -- Fitness is stored to avoid calculating it for each comparison.
      calculateFitness g = (fitness environment g, g)
      getFitness = fst
      getGenotype = snd
      compareFitness = comparing $ Down . fst

      -- Moves k best genotypes to the front, but doesn't sort them further.
      selectBest k vector = selectBy compareFitness vector k

      selected = V.modify (selectBest 1)
               $ V.take numSurvivors
               $ V.modify (selectBest numSurvivors)
               $ V.map calculateFitness population

   in ( getFitness $ V.head selected
      , V.map getGenotype selected
      )

shouldContinue :: [Double] -- ^ Fitness history
               -> Bool
shouldContinue (x:y:_) = x /= y
shouldContinue _       = True

evolutionLoop :: Species e g
              => e -- ^ Environment
              -> [Double] -- ^ Fitness history
              -> V.Vector g -- ^ Survivors from previous generation
              -> IO (V.Vector g) -- ^ Final population
evolutionLoop environment history survivors =
  do
    population <- evolvePopulation environment survivors

    let (bestFitness, survivors') = selectSurvivors environment population
        history' = bestFitness : history

    printf "Generation: %3i  Fitness: %7.1f\n"
      (length history') (head history')

    if shouldContinue history'
      then evolutionLoop environment history' survivors'
      else return survivors'

-- | Run the genetic algorithm.
evolve :: Species e g
       => e -- ^ Environment
       -> IO g -- ^ Optimal genotype
evolve environment = do
  population <- initialPopulation environment
  survivors <- evolutionLoop environment [] population
  return $ V.head survivors
