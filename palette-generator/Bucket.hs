{-# LANGUAGE ScopedTypeVariables #-}

module Bucket
  ( Bucket
  , emptyBucket
  , insertToBucket
  , bucketSize
  , bucketAverage
  , Buckets
  , emptyBuckets
  , makeBuckets
  , makeBuckets'
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import RGBHSV ( HSV(HSV) )

data Bucket a = Bucket Int a a a

emptyBucket :: (Num a) => Bucket a
emptyBucket = Bucket 0 0 0 0

insertToBucket :: (Num a) => HSV a -> Bucket a -> Bucket a
insertToBucket (HSV h s v) (Bucket count h' s' v')
  = Bucket (count + 1) (h' + h) (s' + s) (v' + v)

bucketSize :: Bucket a -> Int
bucketSize (Bucket size _ _ _) = size

bucketAverage :: (Fractional a) => Bucket a -> HSV a
bucketAverage (Bucket size h' s' v')
  = HSV (h' / size') (s' / size') (v' / size')
  where size' = fromIntegral size

type Buckets a = Map Int (Bucket a)

emptyBuckets :: (Num a) => Buckets a
emptyBuckets = Map.fromList [(x, emptyBucket) | x <- [0..8]]

makeBuckets :: forall a. (Fractional a, Num a, RealFrac a) =>
               (HSV a -> a) -> Int -> [HSV a] -> Buckets a
makeBuckets f numberOfBuckets = foldr allocateToBucket emptyBuckets

  where allocateToBucket :: (Fractional a, Num a, RealFrac a) =>
                            HSV a -> Buckets a -> Buckets a
        allocateToBucket colour = Map.adjust (insertToBucket colour) bucket
          where bucket = floor $ fromIntegral numberOfBuckets * f colour

makeBuckets' :: forall a. (Fractional a, Num a, RealFrac a) =>
                (HSV a -> a) -> Int -> [HSV a] -> [Bucket a]
makeBuckets' f numberOfBuckets = Map.elems . makeBuckets f numberOfBuckets
