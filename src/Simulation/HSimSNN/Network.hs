-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes

-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: Population, connections:: Connections}
               deriving Show


-- | Pass a set of spikes through a network and get the network state and spikes
--
-- The basic steps of simulation/transformation are
--
-- - Check for the smallest 'timeOfNextSpike' spike in the 'population'
--
-- - Compare this to the first spike of SpikeTrain
--      
--      - Apply which ever spike occors first to the network
--
--      - Append any output spikes to the response 'SpikeTrain'
--
-- - Repeat the same process again
--
-- - Stop when
--
--      - input 'SpikeTrain' is empty 
--
--      - and all 'timeOfNextSpike' in the network are 'Never'
--
--      - Or when time is larger than some limit
--
-- - Return the final 'SpikeTrain' and 'Network'
passThroughNetwork:: SpikeTrain -> Network -> (SpikeTrain, Network)
passThroughNetwork st network = (st, network)
