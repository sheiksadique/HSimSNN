-- | Neuron module encapsulates behavior of a 'Neuron'
--
-- Some considerations for event driven simulation of SNN
--
-- * Given current 'state' of 'Neuron', it should be possible to predict the time at
-- which it will generate a spike (if any)
--
-- * For a synapse model with dynamics it is possible that the neuron fires in the
-- future, so such synapses should be part of the Neuron data type.
--
module Simulation.HSimSNN.Neuron where

-- Neuron is defined by its state and time at which its state was last evaluated
data Neuron = Neuron {state:: Double, tlastupdate::Double}
              deriving Show

-- Initializes a neuron with a given state at time 0
initNeuron st = Neuron st 0

-- | Returns the membrane potential of a neuron
vmem:: Neuron -> Double
vmem neuron = state neuron -- For now the state of a neuron is its memrane potential

-- | Checks if the membrane potential of a neuron is above threshold value
aboveThreshold:: Neuron -> Double -> Bool
aboveThreshold neuron threshold
        | threshold > vmem neuron = False
        | otherwise = True


-- | Evaluate state of neuron at time t
-- Ideally used at the arrival of a spike or when the neuron spikes (when an
-- event occoured)
evaluateStateAtt:: Neuron -> Double -> Neuron
evaluateStateAtt neuron t = Neuron newstate t
                         where
                             newstate = state neuron -- Should be the updated state at time t based on state at tlastupdate
