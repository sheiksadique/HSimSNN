-- | Module for handling spikes
--
module Simulation.HSimSNN.Spikes where

import qualified Data.Vector.Unboxed as V

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
-- Note that the index is Int and not Double
data SpikeTrain = SpikeTrain (V.Vector (Int, Double))
                  deriving Show
