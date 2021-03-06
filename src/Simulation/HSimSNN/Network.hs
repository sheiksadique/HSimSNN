{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.Matrix.Unboxed as M
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Neuron
import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Spikes


-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: !Population, connections:: !Connections}
               deriving (Show, Generic)

instance NFData Network

-- | Value of network state
type NetworkValue = SpikeTrain

-- | Network state
type NetworkState = (Network, SpikeTrain)

-- | NetworkValue alias - delayed spikes after tsim
type DelayedSpikes = SpikeTrain

-- | State based resetNeuronNinNet
resetNeuronNinNet :: Int -> Double -> Network -> Network
resetNeuronNinNet n t network =
    let updtPop = resetNeuronOfPop (population network) (Just n) t
    in Network updtPop (connections network)

-- | Apply a presynaptic spike to a neuron at time t
-- applyPreSynapticSpike :: (Int, Double) -> SynInfo -> State NetworkState ()
applyPreSynapticSpike :: (Int, Double) -> SynInfo -> Network -> Network
applyPreSynapticSpike spk syninfo (Network pop conn) =
    let updtPop = applyPreSynapticSpikeToPop spk syninfo pop
    in Network updtPop conn

passThroughNetwork :: Network -> SpikeTrain -> Double -> NetworkState
passThroughNetwork net spkTrain tsim =
    execState
        (go spkTrain tsim)
        (net,EmptySpikeTrain)
  where
    delay = 1.0 -- Spike transmission delay hardcoded
    dt = 0.1 -- Hardcoded time window

    go :: SpikeTrain -> Double -> State NetworkState DelayedSpikes
    go EmptySpikeTrain tsim = do
        (network,spkout) <- get
        case firstSpikingNeuron
                 (population network) of
            Nothing -> return EmptySpikeTrain
            Just indx ->
                if (nextspktm > At tsim) -- next spike time after tsim
                    then return EmptySpikeTrain
                    else do
                        let newnet = resetNeuronNinNet indx tn network
                        put (newnet, concST spkout newspk)
                        go (SpikeTrain
                                   (VU.singleton
                                        (Spike (indx, tn + delay))))
                           tsim
                where nextspktm =
                          nextSpikeTime
                              ((neurons . population) network V.!
                               indx)
                      tn = getTime nextspktm
                      newspk =
                          SpikeTrain
                              (VU.singleton
                                   (Spike (indx, tn)))
    go (SpikeTrain spktrn) tsim = do
        let Spike (indx,t) =
                VU.head spktrn -- First spike
        if (t <= tsim)
            then do
                --update network to before the spike arrives and collect any delayed
                --spikes
                dspktrn <- go EmptySpikeTrain t -- Delayed spikes
                -- Apply all spikes within a time period of deltat
                (net,spkout) <- get
                let (net', restspk) = applyAllSpikesWithinWindow net (SpikeTrain spktrn) t dt
                put (net',spkout)
                -- Process reminder spikes
                go (mergeST dspktrn restspk)
                   tsim
            else do
                -- Input spikes arrive after simulation time so they don't matter
                dspktrn <-
                    go EmptySpikeTrain tsim
                return (mergeST dspktrn (SpikeTrain spktrn))


-- | Update all neurons connected to this axon
updateAllNeuronsFromAxon :: Network -> Int -> Double -> Network
updateAllNeuronsFromAxon net aIx t =
    let synInfoRow = (M.takeRow ((synInfo . connections) net) aIx)
    in VU.foldl' f net synInfoRow
  where f net (sIx, sInfo) = applyPreSynapticSpike (sIx, t) sInfo net


-- | Apply spikes from a 'SpikeList' to 'Network' withink time window and returns the residual spikes to be evaluated
applyAllSpikesWithinWindow :: Network -> SpikeTrain -> Double -> Double -> (Network, SpikeTrain)
applyAllSpikesWithinWindow net st@(SpikeTrain spktrn) tstart dt
        | isEmptySpikeTrain st = (net, EmptySpikeTrain)
        | (dt == 0) = (net', (SpikeTrain restspk))
        | (t < tstart+dt) = applyAllSpikesWithinWindow net' (SpikeTrain restspk) tstart dt
        | otherwise = (net, st)
        where
            Spike (indx,t) = VU.head spktrn -- First spike
            restspk = VU.tail spktrn -- reminder of spikes
            net' = updateAllNeuronsFromAxon net indx tstart
