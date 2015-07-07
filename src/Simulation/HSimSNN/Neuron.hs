
-- | Neuron module encapsulates behavior of a Neuron
module Simulation.HSimSNN.Neuron where

data Neuron = Neuron {vmem:: Double}
              deriving Show

-- | Checks if the membrane potential of a neuron is above threshold value
aboveThreshold:: Neuron -> Double -> Bool
aboveThreshold neuron threshold
        | threshold > vmem neuron = False
        | otherwise = True
