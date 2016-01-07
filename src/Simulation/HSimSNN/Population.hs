{-# LANGUAGE ViewPatterns #-}
-- | Population module encompasses all functions on populations
module Simulation.HSimSNN.Population where

import Simulation.HSimSNN.Neuron
import Simulation.HSimSNN.Spikes
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | A populatoin of 'Neuron's is simply a list of 'Neuron's
data Population = Population {neurons:: !(Vector Neuron)}
                  deriving Show

-- | Initialize a population of neurons with a list of initial states
initPop :: Vector [Double] -> Population
initPop st = Population $ V.map initNeuron st


-- | Evaluate states of a population of neurons
-- This method should *not* ideally be called for every neuron in the global population but only only to the subpopulation that a spike event corresponds to.
evaluatePopStateAtt:: Population -> Double -> Population
evaluatePopStateAtt (Population ns) t = Population $ V.map (flip evaluateNeuronStateAtt t) ns


-- | Find the index of neuron that is about to spike in a 'Population'
firstSpikingNeuron :: Population -> Maybe Int
firstSpikingNeuron pop
  | firstspiketime == Never = Nothing
  | otherwise =
      V.findIndex
          (\x ->
                (nextSpikeTime x) ==
                firstspiketime)
          (neurons pop)
  where
    firstspiketime =
        minimum (V.map nextSpikeTime (neurons pop))

-- | Update the neuron with index i of a pop at time t
resetNeuronOfPop :: Population -> Maybe Int -> Double -> Population
resetNeuronOfPop p Nothing _ = p
resetNeuronOfPop (Population ns) (Just i) t =
    Population (V.modify (\v -> VM.modify v (flip resetNeuron t) i) ns)

-- | convenience function to apply spike to population
applyPreSynapticSpikeToPop :: (Int, Double)
                           -> SynInfo
                           -> Population
                           -> Population
applyPreSynapticSpikeToPop (i,spktm) syninfo (Population ns) =
    Population
        (V.modify
             (\v ->
                   VM.modify v (applySynapticSpikeToNeuron syninfo spktm) i)
             ns)
