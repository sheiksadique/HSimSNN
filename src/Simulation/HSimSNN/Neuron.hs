{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Control.DeepSeq
import qualified Data.Vector.Generic.Mutable as VM
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import qualified Simulation.HSimSNN.Spikes as SPK

-- | Data container for synaptic information related to a connection
data SynType = Exec deriving (Show,Read,Enum,Generic) 

instance NFData SynType

data SynInfo = SynInfo
    { weight :: {-# UNPACK #-} !Double
    , syntype :: !SynType
    } deriving (Show, Generic)

instance NFData SynInfo

derivingUnbox "SynInfo"
    [t| SynInfo → (Double, Int) |]
    [| \ (SynInfo weight syntype) → (weight, (fromEnum syntype)) |]
    [| \ (weight, syntype) → (SynInfo weight (toEnum syntype)) |]

-- | Neuron threshold
threshold :: Double
threshold = 1.0

-- | Neuron is defined by its state and time at which its state was last evaluated
-- The state of a neuron is defined as list of doubles
-- Try unboxed Vector for better performance

data Neuron = Neuron
    { state :: !(V.Vector Double)
    , tLastUpdate :: {-# UNPACK #-} !Double
    } deriving (Generic)

instance NFData Neuron

-- | String representation for Neuron
instance Show Neuron where
    show (Neuron st tl) = "Neuron (" ++ (show $ (V.toList) st) ++ " @ " ++ (show tl) ++ ")"

-- | Initializes a neuron with a given state at time 0
initNeuron :: [Double] -> Neuron
initNeuron st = Neuron (V.fromList st) 0

-- | Returns the membrane potential of a neuron

-- Maybe use Non-empty Vector?
vmem:: Neuron -> Double
vmem neuron = (V.head.state) neuron -- For now the state of a neuron is the first state variable

-- The below block of functions all effect the dynamics of the neuron

-- | Checks if the membrane potential of a neuron is above threshold value
-- use a reader monad for global values?
aboveThreshold:: Neuron -> Bool
aboveThreshold neuron = vmem neuron > threshold
{-# INLINE aboveThreshold #-}

-- | Check for threshold and reset neuron
-- Should be called with the simulatoin time and only when the neuron spikes
-- Perhaps this should be an internal/hidden function ?
-- Hardcasting threshold to 1.0 TODO: should parametrize somehow
-- See above use reader monad for parameters
resetNeuron:: Neuron -> Double -> Neuron
resetNeuron neuron t
                | tLastUpdate neuron > t = error $
                                             (show t)
                                             ++ "Neuron has already been updated to the future"
                                             ++ (show $ tLastUpdate neuron) -- for debugging
                | otherwise = Neuron newstate t
                where
                    -- Rewrite this without lists
                    y = V.tail $ state neuron -- time of last spike
                    -- neuron dynamics
                    newstate = [0,t] V.++ y
{-# INLINE resetNeuron #-}

-- | Evaluate the next possible spike time of a neuron given its state at time t
--
-- This function is essentially what defines the dynamics of the neuron. (not really.. it depends on the dynamics though)
-- Currently the neuron receives a constant input current
-- Ideally this should be something users can define and pass at the top level
nextSpikeTime:: Neuron -> SPK.NextSpikeTime
nextSpikeTime neuron
    | aboveThreshold neuron = SPK.At $ tLastUpdate neuron
    | otherwise = SPK.Never
    -- -- |otherwise =  SPK.At $(threshold-vmem neuron) + tlastupdate neuron
{-# INLINE nextSpikeTime #-}


-- | Evaluate state of neuron at time t
-- Ideally used at the arrival of a spike or when the neuron spikes (when an
-- event occoured)
evaluateNeuronStateAtt:: Neuron -> Double -> Neuron
evaluateNeuronStateAtt neuron t
                | t > (tLastUpdate neuron) = Neuron newstate t
                | t == (tLastUpdate neuron) = neuron -- The neuron has already been updated
                | otherwise = error $ (show t)
                                      ++ "Neuron has already been updated to the future"
                                      ++ (show $ tLastUpdate neuron) -- for debugging
                where
                    taum = 10.0
                    decayfact = exp (((tLastUpdate neuron)-t)/taum) -- decay factor
                    -- Rewrite this without lists
                    newstate =
                        V.modify
                            (\v ->
                                  VM.modify v (* decayfact) 0)
                            (state neuron)
{-# INLINE evaluateNeuronStateAtt #-}

-- | Apply a presynaptic spike to a neuron at time t
applySynapticSpikeToNeuron :: SynInfo -> Double -> Neuron -> Neuron
applySynapticSpikeToNeuron (SynInfo w _) spktm neuron
                | isRefractoryAtt neuron spktm = Neuron curstate spktm
                | otherwise = Neuron newstate spktm
                where
                    Neuron curstate _ = evaluateNeuronStateAtt neuron spktm
                    newstate = V.modify (\v -> VM.modify v (+w) 0) curstate
{-# INLINE applySynapticSpikeToNeuron #-}

-- | Check if a neuron is still refractory
isRefractoryAtt:: Neuron -> Double -> Bool
isRefractoryAtt (Neuron oldstate tlastupdate) t
                | (t-tlastupdate) > tref  = False -- Neuron has not been modified within refractory time window
                | (t-(oldstate V.! 1)) > tref = False -- last spike time was before refractory time window
                | otherwise = True
                where
                    tref = 0.5
{-# INLINE isRefractoryAtt #-}
