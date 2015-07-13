-- | Population module encompasses all functions on populations
module Simulation.HSimSNN.Population where

import Simulation.HSimSNN.Neuron
import Simulation.HSimSNN.Spikes
import Data.List

-- | A populatoin of 'Neuron's is simply a list of 'Neuron's
data Population = Population {neurons::[Neuron]}
                  deriving Show

-- | Initialize a population of neurons with a list of initial states
initPop st = Population $ map initNeuron st


-- | Evaluate states of a population of neurons
-- This method should *not* ideally be called for every neuron in the global population but only only to the subpopulation that a spike event corresponds to.
evaluatePopStateAtt:: Population -> Double -> Population
evaluatePopStateAtt (Population neurons) t = Population $ map (flip evaluateNeuronStateAtt t) neurons


-- | Find the index of neuron that is about to spike in a 'Population'
firstSpikingNeuron:: Population -> Maybe Int
firstSpikingNeuron pop
    | firstspiketime == Never = Nothing
    | otherwise = findIndex (\x -> (nextSpikeTime x)==firstspiketime) $neurons pop
    where    
        firstspiketime = minimum $map nextSpikeTime $neurons pop


-- | Update the neuron with index i of a pop at time t
resetNeuronOfPop:: Population -> Maybe Int -> Double -> Population
resetNeuronOfPop pop Nothing t = pop
resetNeuronOfPop pop (Just i) t = updtpop
        where
            ns = neurons pop
            updtpop = Population $ map (\z -> if z==i then 
                                                (resetNeuron (ns !! z) t)
                                              else 
                                                (ns !! z)) 
                                       [0..((length ns)-1)]
