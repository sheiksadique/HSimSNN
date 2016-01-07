{-# LANGUAGE ViewPatterns #-}
-- | Population module encompasses all functions on populations
module Simulation.HSimSNN.Population where

import Simulation.HSimSNN.Neuron
import Simulation.HSimSNN.Spikes
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | A populatoin of 'Neuron's is simply a list of 'Neuron's
data Population = Population {neurons:: !(Vector Neuron)}
                  deriving Show

-- | Initialize a population of neurons with a list of initial states
initPop st = Population $ V.map initNeuron st


-- | Evaluate states of a population of neurons
-- This method should *not* ideally be called for every neuron in the global population but only only to the subpopulation that a spike event corresponds to.
evaluatePopStateAtt:: Population -> Double -> Population
evaluatePopStateAtt (Population neurons) t = Population $ V.map (flip evaluateNeuronStateAtt t) neurons


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
resetNeuronOfPop pop Nothing _ = pop
resetNeuronOfPop pop (Just i) t =
    let ns = neurons pop
    in Population
            (V.map
                 -- TODO: Refactor these for readability
                 (\z -> if z == i
                           then resetNeuron (ns V.! z) t
                           else (ns V.! z))
                 (V.fromList
                      [0 .. ((length ns) - 1)]))

-- | convenience function to apply spike to population
-- Ugly in vector form, why not use V.modify?
applyPreSynapticSpikeToPop :: (Int, Double)
                           -> SynInfo
                           -> Population
                           -> Population
applyPreSynapticSpikeToPop (indx,spktm) syninfo (Population p) = Population (V.modify
    (\v -> VM.modify v (applySynapticSpikeToNeuron syninfo
                                                   spktm)
                              indx

    ) p)
    -- Population (a V.++ (V.cons nl b))
  -- where
  --   -- TODO: Partial function
  --   (a,(V.splitAt 1 -> (z,b))) =
  --       V.splitAt indx (p)
  --   nl = applySynapticSpikeToNeuron syninfo spktm (V.head z)
