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
                      delay = 1.0 -- Spike transmission delay hardcoded
    go (SpikeTrain spktrn) tsim = do
        let Spike (indx,t) =
                VU.head spktrn -- First spike
        if (t <= tsim)
            then do
                --update network to before the spike arrives and collect any delayed
                --spikes
                dspktrn <- go EmptySpikeTrain t -- Delayed spikes
                -- Apply first spike
                (net,spkout) <- get
                let net' = updateAllNeurons net indx t
                put (net',spkout)
                -- Process reminder spikes
                let restspk = VU.tail spktrn -- reminder of spikes
                go (mergeST dspktrn (SpikeTrain restspk))
                   tsim
            else do
                -- Input spikes arrive after simulation time so they don't matter
                dspktrn <-
                    go EmptySpikeTrain tsim
                return (mergeST dspktrn (SpikeTrain spktrn))

passThroughNetwork' :: NetworkState -> SpikeTrain -> Maybe Int -> Double -> NetworkState
passThroughNetwork' netState _ Nothing _ = netState
passThroughNetwork' (net,spkout) EmptySpikeTrain (Just idx) tSim =
    if (tNext > tSim) -- next spike time after tsim
        then (net,spkout)
        else let net' = resetNeuronNinNet idx tNext net
             in (net', concST spkout newspk)
  where tNext = getTime (nextSpikeTime
                            ((neurons . population) net V.!
                            idx))
        newspk = SpikeTrain (VU.singleton (Spike (idx, tNext)))

passThroughNetwork' (net,spkout) (SpikeTrain spktrn) (Just idx) tSim =
    let Spike (aIx,t) = VU.head spktrn -- First spike
    in if (t <= tSim)
            then let net' = updateAllNeurons net aIx t
                 in (net', spkout)
            else (net, spkout)

passThroughSpikes :: NetworkState -> Maybe Int -> Double -> DelayedSpikes
passThroughSpikes _ Nothing _ = EmptySpikeTrain
passThroughSpikes (net, EmptySpikeTrain) (Just idx) tsim =
    if (tNext > tsim) -- next spike time after tsim
        then EmptySpikeTrain
        else undefined
  where
    tNext = getTime (nextSpikeTime
                        ((neurons . population) net V.!
                       idx))
    newspk = SpikeTrain (VU.singleton (Spike (idx, tNext)))
    delay = 1.0 -- Spike transmission delay hardcoded
passThroughSpikes (net, (SpikeTrain spktrn)) (Just idx) tsim = do
           let Spike (indx,t) =
                   VU.head spktrn -- First spike
           if (t <= tsim)
               then undefined
                   --dspktrn <- go EmptySpikeTrain t -- Delayed spikes
                   -- Process reminder spikes
                   -- let restspk = VU.tail spktrn -- reminder of spikes
                   -- go (mergeST dspktrn (SpikeTrain restspk))
                   --    tsim
               else undefined -- Input spikes arrive after simulation time so they don't matter
                   -- dspktrn <-
                   --     go EmptySpikeTrain tsim
                   -- return (mergeST dspktrn (SpikeTrain spktrn))



-- | Update all neurons connected to this axon
updateAllNeurons :: Network -> Int -> Double -> Network
updateAllNeurons net aIx t =
    let synInfoRow = (M.takeRow ((synInfo . connections) net) aIx)
    in VU.foldl' f net synInfoRow
  where f net (sIx, sInfo) = applyPreSynapticSpike (sIx, t) sInfo net
