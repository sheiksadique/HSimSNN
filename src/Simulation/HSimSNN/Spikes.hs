-- | Module for handling spikes
--
module Simulation.HSimSNN.Spikes where

import qualified Data.Vector as V


-- | Spike is a tuple of the form (index, time)
data Spike = Spike (Int, Double)
instance Show Spike where
    show (Spike (x,y)) = show (x,y)
instance Eq Spike where
    (==) (Spike (_,t1)) (Spike (_,t2)) = (t1==t2)
instance Ord Spike where
    (<=) (Spike (_,t1)) (Spike (_,t2)) = (t1<=t2)


-- | SpikeTrain data type consists of a vector of tuples of the form (index, time)
-- You can initialize a SpikeTrain as follows
--
-- @
--      > import qualified Data.Vector.Unboxed as V
--      > let indx = V.fromList [0..20]
--      > let spktm = V.fromList [5.0,5.1..7]
--      > let spktrain = SpikeTrain $ V.zip indx spktm
-- @
--
-- Note 1 - the index is Int and not Double
-- Note 2 - TODO: SpikeTrain (V.fromList []) != EmptySpikeTrain although conceptually it is.
data SpikeTrain = SpikeTrain (V.Vector (Int, Double)) | EmptySpikeTrain
                  deriving (Show, Eq)


-- | Concatenate two 'SpikeTrain's
concST :: SpikeTrain -> SpikeTrain -> SpikeTrain
concST EmptySpikeTrain st = st
concST st EmptySpikeTrain = st
concST (SpikeTrain v1) (SpikeTrain v2) = SpikeTrain (v1 V.++ v2)


-- | Represents next time of spike of a neuron
data NextSpikeTime = At Double | Never
instance Eq NextSpikeTime where
    (==) Never Never = True
    (==) Never (At t) = False
    (==) (At t) Never = False
    (==) (At t1) (At t2) = (t1 == t2)
instance Ord NextSpikeTime where
    (<=) Never Never = True
    (<=) Never (At t) = False
    (<=) (At t) Never = True
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
spikeTrainFromString str = SpikeTrain $V.fromList (read str :: [(Int, Double)])

