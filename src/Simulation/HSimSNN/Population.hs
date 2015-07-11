-- | Population module encompasses all functions on populations
module Simulation.HSimSNN.Population where

import Simulation.HSimSNN.Neuron

-- | A populatoin of 'Neuron's is simply a list of 'Neuron's
data Population = Population {neurons::[Neuron]}
                  deriving Show

-- | Initialize a population of neurons with a list of initial states
initPop st = Population $ map initNeuron st


-- | Evaluate states of a population of neurons
-- This method should *not* ideally be called for every neuron in the global population but only only to the subpopulation that a spike event corresponds to.
evaluatePopStateAtt:: Population -> Double -> Population
evaluatePopStateAtt (Population neurons) t = Population $ map (flip evaluateNeuronStateAtt t) neurons
