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

import qualified Data.Vector.Unboxed as V
import qualified Simulation.HSimSNN.Spikes as SPK

-- | Data container for synaptic information related to a connection
data SynInfo = SynInfo {weight::Double, syntype::String}
               deriving Show


-- | Neuron threshold
threshold = 5.0


-- | Neuron is defined by its state and time at which its state was last evaluated
-- The state of a neuron is defined as list of doubles
data Neuron = Neuron {state::V.Vector Double, tlastupdate::Double, tnextspike::SPK.NextSpikeTime}
-- | String representation for Neuron
instance Show Neuron where
    show (Neuron st tl _) = "Neuron (" ++ (show $(V.toList) st) ++ " @ " ++ (show tl) ++ ")"

-- | Initializes a neuron with a given state at time 0
initNeuron st = Neuron (V.fromList st) 0 SPK.Never -- TODO: Hardcoding Never incorrect

-- | Returns the membrane potential of a neuron
vmem:: Neuron -> Double
vmem neuron = (V.head.state) neuron -- For now the state of a neuron is the first state variable

-- The below block of functions all effect the dynamics of the neuron


-- | Evaluate state of neuron at time t
-- Ideally used at the arrival of a spike or when the neuron spikes (when an
-- event occoured)
evaluateNeuronStateAtt:: Neuron -> Double -> Neuron
evaluateNeuronStateAtt neuron t 
                |t == (tlastupdate neuron) = neuron -- The neuron has already been updated
                |t > (tlastupdate neuron) = Neuron newstate t SPK.Never -- TODO: Hardcoding Never .. incorrect
                |otherwise = error "This neuron has already been updated to the future"
                where
                    decayfact = exp ((tlastupdate neuron)-t) -- decay factor
                    newstate = V.map (*decayfact) $ state neuron -- neuron dynamics



-- | Checks if the membrane potential of a neuron is above threshold value
aboveThreshold:: Neuron -> Bool
aboveThreshold neuron
        | threshold > vmem neuron = False
        | otherwise = True



-- | Check for threshold and reset neuron
-- Should be called with the simulatoin time and only when the neuron spikes
-- Perhaps this should be an internal/hidden function ?
-- Hardcasting threshold to 1.0 TODO: should parametrize somehow
resetNeuron:: Neuron -> Double -> Neuron
resetNeuron neuron t 
                |tlastupdate neuron > t = error "Neuron has already been updated to the future"
                |aboveThreshold neuron = Neuron newstate t SPK.Never
                |otherwise = error "Resetting neuron below threshold"
                where
                    newstate = V.map (*0) $ state neuron -- neuron dynamics
                  

-- | Evaluate the next possible spike time of a neuron given its state at time t
nextSpikeTime:: Neuron -> SPK.NextSpikeTime
nextSpikeTime neuron
    |aboveThreshold neuron = SPK.At $ tlastupdate neuron
    |otherwise = SPK.Never
