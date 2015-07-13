-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes
import Simulation.HSimSNN.Neuron

import Data.Maybe
import qualified Data.Vector.Unboxed as V

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
--      - Apply which ever spike occors first to the network through 'connections'
--
--      - Append any output spikes to the response 'SpikeTrain'
--
-- - Repeat the same process again.
-- - Stop when
--
--      - input 'SpikeTrain' is empty 
--
--      - and all 'timeOfNextSpike' in the network are 'Never'
--
--      - Or when time is larger than some limit
--
-- - Return the final 'SpikeTrain' and 'Network'
passThroughNetwork:: SpikeTrain -> Network -> Double -> (SpikeTrain, Network)
-- Instance for no input just spontanious activity
passThroughNetwork emptySpikeTrain network tsim
    | (i==Nothing) = (emptySpikeTrain, network)
    | otherwise = (outspk, newnetwork)
    where
        -- - Check for the smallest 'timeOfNextSpike' spike in the 'population'
        i = firstSpikingNeuron (population network)
        indx = fromJust i
        (spktm, newnetwork) = extractSpike network indx
        outspk = SpikeTrain $ V.fromList [(indx,spktm)]
-- Instance for when there is activity
passThroughNetwork spktrn network tsim = (spktrn, network)

-- | Extract spike from the 'n'th neuron in a Network.
extractSpike:: Network -> Int -> (Double, Network)
extractSpike network indx = (getTime t, newnetwork)
        where
            t = nextSpikeTime $ ((neurons.population) network)!! indx
            updtpop = resetNeuronOfPop (population network) (Just indx) (getTime t)
            newnetwork = Network updtpop (connections network)
