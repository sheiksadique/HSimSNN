-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes
import Simulation.HSimSNN.Neuron

import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad.State

-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: Population, connections:: Connections}
               deriving Show


type NetworkValue = SpikeTrain
type NetworkState = (Network, SpikeTrain)


-- | State monad based implementation of the function
passThroughNetwork' :: SpikeTrain -> Double -> State NetworkState NetworkValue
passThroughNetwork' EmptySpikeTrain tsim = do
    (network, spkout) <- get
    put (network, (SpikeTrain (V.fromList [(100,0.3)])) )
    return EmptySpikeTrain
            
            

-- dummy function doesn't do anything for now
passThroughNetwork' spktrn tsim = do
    (network, spkout) <- get
    put (network, (SpikeTrain (V.fromList [(100,0.3)])) )
    return EmptySpikeTrain
    


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
-- When there is no input
passThroughNetwork EmptySpikeTrain network tsim 
    | (i==Nothing) = (EmptySpikeTrain, network) -- no spontanious spikes 
    | (nextspktm>At tsim) = (EmptySpikeTrain, network)-- next spike time after tsim
    | otherwise = (outspk, newnetwork2) -- spont activity
    where
        -- - Check for the smallest 'timeOfNextSpike' spike in the 'population'
        i = firstSpikingNeuron (population network)
        indx = fromJust i
        nextspktm = nextSpikeTime $ ((neurons.population) network)!! indx
        newnetwork = resetNeuronNinNet network indx (getTime nextspktm)
        (spktrn,newnetwork2) = passThroughNetwork EmptySpikeTrain newnetwork tsim
        outspk = concST (SpikeTrain $V.fromList [(indx,getTime nextspktm)]) spktrn
-- When there is input
passThroughNetwork spktrn network tsim = (SpikeTrain (V.fromList [(100,0.3)]), network) -- dummy for now


-- | reset 'n'th neuron in a Network at time t.
resetNeuronNinNet:: Network -> Int -> Double -> Network
resetNeuronNinNet network n t= newnetwork
        where
            updtpop = resetNeuronOfPop (population network) (Just n) t
            newnetwork = Network updtpop (connections network)
