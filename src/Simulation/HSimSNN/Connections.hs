{-# LANGUAGE DeriveGeneric #-}
-- | Connections module describes all elements of network connectivity

module Simulation.HSimSNN.Connections where

import Control.DeepSeq
import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Neuron
import GHC.Generics (Generic)
import Data.Matrix.Unboxed (Matrix)

-- | Connections encapsulates information about source and destination neurons
-- and corresponding synaptic information
--
-- * The connections are essentially a matrix from source to destination neurons.
--
-- * The index of the top level list of syninfo corresponds to the index of source neurons - (0,N). Therefore, it is expected to have the same length as pop.
--
-- * The first int in the tuple is the index of destination neurons - (0..N).
--
-- * The length of each sublist can be more than N to support multiple synapses per neuron.
--
data Connections = Connections
    { pop :: !Population
    , synInfo :: !(Matrix (Int, SynInfo))
    } deriving Generic

-- | String representation of Connections
instance Show Connections where
    show = show . synInfo

instance NFData Connections
