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
threshold = 1.0


-- | Neuron is defined by its state and time at which its state was last evaluated
-- The state of a neuron is defined as list of doubles
data Neuron = Neuron {state::V.Vector Double, tlastupdate::Double}
-- | String representation for Neuron
instance Show Neuron where
    show (Neuron st tl) = "Neuron (" ++ (show $(V.toList) st) ++ " @ " ++ (show tl) ++ ")"

-- | Initializes a neuron with a given state at time 0
initNeuron st = Neuron (V.fromList st) 0 -- TODO: Hardcoding Never incorrect

-- | Returns the membrane potential of a neuron
vmem:: Neuron -> Double
vmem neuron = (V.head.state) neuron -- For now the state of a neuron is the first state variable

-- The below block of functions all effect the dynamics of the neuron





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
                |tlastupdate neuron > t = error "Neuron has already been updated to the future" -- for debugging
                |otherwise = Neuron newstate t
                where
                    newstate = V.map (*0) $ state neuron -- neuron dynamics
                  

-- | Evaluate the next possible spike time of a neuron given its state at time t
-- 
-- This function is essentially what defines the dynamics of the neuron. (not really.. it depends on the dynamics though)
-- Currently the neuron receives a constant input current
-- Ideally this should be something users can define and pass at the top level
nextSpikeTime:: Neuron -> SPK.NextSpikeTime
nextSpikeTime neuron
    |aboveThreshold neuron = SPK.At $ tlastupdate neuron
    |otherwise = SPK.Never
    -- -- |otherwise =  SPK.At $(threshold-vmem neuron) + tlastupdate neuron 


-- | Evaluate state of neuron at time t
-- Ideally used at the arrival of a spike or when the neuron spikes (when an
-- event occoured)
evaluateNeuronStateAtt:: Neuron -> Double -> Neuron
evaluateNeuronStateAtt neuron t 
                |t == (tlastupdate neuron) = neuron -- The neuron has already been updated
                |t > (tlastupdate neuron) = Neuron newstate t
                |otherwise = error "This neuron has already been updated to the future"
                where
                    decayfact = exp ((tlastupdate neuron)-t) -- decay factor
                    newstate = V.map (*decayfact) $ state neuron -- neuron dynamics


-- | Apply a presynaptic spike to a neuron at time t
applySynapticSpikeToNeuron :: SynInfo -> Double -> Neuron -> Neuron
applySynapticSpikeToNeuron (SynInfo w typ) spktm neuron = Neuron newstate spktm
    where
        Neuron curstate _ = evaluateNeuronStateAtt neuron spktm
        newstate = V.fromList [(V.head) newstate + w] V.++ ((V.tail) newstate)


