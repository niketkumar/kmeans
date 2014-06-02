{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Chapter6.Kmeans where

import Data.Foldable (minimumBy)
import Control.Lens
import qualified Data.Map as M
import Data.Default

data KMeansState e v = KMeansState {_centroids :: [v], _points :: [e],
                                    _err :: Double, _threshold :: Double,
                                    _steps :: Int} deriving Show
makeLenses ''KMeansState

class (Ord v, Default v) => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

instance Vector (Double, Double) where
    distance (a,b) (c,d) = sqrt $ (c-a)^2 + (d-b)^2
    centroid cs = (u/n, v/n) where
                      (u,v) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0.0,0.0) cs
                      n = fromIntegral $ length cs

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id
    
initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state = 
    let assignments = clusterAssignments state
        state1 = state & centroids.traversed %~ 
                                (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
        state2 = state1 & err .~ sum (zipWith distance (state^.centroids) (state1^.centroids))
        state3 = state2 & steps +~ 1
    in if state3^.err < state3^.threshold then state3 else kMeans' state3

clusterAssignments :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignments state = 
    let cs = state^.centroids
        initialMap = M.fromList $ zip cs (repeat [])
    in foldr (\p m -> 
                let c = minimumBy 
                            (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) 
                            cs
                in M.adjust (p:) c m) 
             initialMap (state^.points)

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v