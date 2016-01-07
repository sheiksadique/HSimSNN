{-# LANGUAGE DeriveGeneric #-}
-- | Module for handling spikes
--
module Simulation.HSimSNN.Spikes where

import qualified Data.Vector as V
import GHC.Generics
import Control.DeepSeq


-- | Spike is a tuple of the form (index, time)
data Spike = Spike {-# UNPACK #-} !(Int, Double) deriving (Generic)

instance Show Spike where
    show (Spike (x,y)) = show (x,y)

instance Eq Spike where
    (==) (Spike (_,t1)) (Spike (_,t2)) = (t1==t2)

instance Ord Spike where
    (<=) (Spike (_,t1)) (Spike (_,t2)) = (t1<=t2)

instance NFData Spike

-- | SpikeTrain data type consists of a vector of Spike
-- You can initialize a SpikeTrain as follows
--
-- @
--      > import qualified Data.Vector.Unboxed as V
--      > let indx = V.fromList [0..20]
--      > let spktm = V.fromList [5.0,5.1..7]
--      > let spktrain = SpikeTrain $ V.map Spike (V.zip indx spktm)
-- @
--
-- Note 1 - the index is Int and not Double
-- Note 2 - TODO: SpikeTrain (V.fromList []) != EmptySpikeTrain although conceptually it is.
data SpikeTrain = SpikeTrain !(V.Vector Spike) | EmptySpikeTrain
                  deriving (Show, Eq, Generic)

instance NFData SpikeTrain where

-- | is equal to EmptySpikeTrain
isEmptySpikeTrain :: SpikeTrain -> Bool
isEmptySpikeTrain EmptySpikeTrain = True
isEmptySpikeTrain (SpikeTrain v1)
   | V.length v1 == 0 = True
   | otherwise = False

-- | Concatenate two 'SpikeTrain's
concST :: SpikeTrain -> SpikeTrain -> SpikeTrain
concST EmptySpikeTrain st = st
concST st EmptySpikeTrain = st
concST (SpikeTrain v1) (SpikeTrain v2) = SpikeTrain (v1 V.++ v2)

-- | Merge two SORTED spike trians
mergeST :: SpikeTrain -> SpikeTrain -> SpikeTrain
mergeST EmptySpikeTrain st = st
mergeST st EmptySpikeTrain = st
mergeST (SpikeTrain v1) (SpikeTrain v2) = SpikeTrain (merge v1 v2)

-- | Merge two sorted lists
-- http://stackoverflow.com/questions/8363445/merge-two-sorted-lists-in-haskell
merge :: Ord a => V.Vector a -> V.Vector a -> V.Vector a
--merge = undefined
merge a b
    | V.length a == 0  = b
    | V.length b == 0  = a
merge a b
    | V.head a <= V.head b = V.cons (V.head a) (merge (V.tail a) b)
    | V.head a > V.head b = V.cons (V.head b) (merge a (V.tail b))

-- | Represents next time of spike of a neuron
data NextSpikeTime = At {-# UNPACK #-} !Double | Never
instance Eq NextSpikeTime where
    (==) Never Never = True
    (==) Never (At _) = False
    (==) (At _) Never = False
    (==) (At t1) (At t2) = (t1 == t2)
instance Ord NextSpikeTime where
    (<=) Never Never = True
    (<=) Never (At _) = False
    (<=) (At _) Never = True
    (<=) (At t1) (At t2) = (t1 <= t2)

-- | Extract time from NextSpikeTime
getTime::NextSpikeTime -> Double
getTime (At t) = t
getTime Never = error "There is no spike"

-- | Save SpikeTrain to a file
spikeTrainToFile:: FilePath -> SpikeTrain -> IO ()
spikeTrainToFile fname EmptySpikeTrain = writeFile fname $ show EmptySpikeTrain
spikeTrainToFile fname (SpikeTrain st) = writeFile fname $ show st

-- | Generate SpikeTrain from file
spikeTrainFromFile:: FilePath -> IO SpikeTrain
spikeTrainFromFile fname = do
    str <- readFile fname
    return $ spikeTrainFromString str

-- | Generate SpikeTrain from String
spikeTrainFromString:: String -> SpikeTrain
spikeTrainFromString "EmptySpikeTrain" = EmptySpikeTrain
spikeTrainFromString str = SpikeTrain $V.fromList $map Spike (read str :: [(Int, Double)])
