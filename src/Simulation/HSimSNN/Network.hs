{-# LANGUAGE DeriveGeneric #-}

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


-- | State based resetNeuronNinNet
resetNeuronNinNet :: Int -> Double -> State NetworkState NetworkValue
resetNeuronNinNet n t = do
    (network, spk) <- get
    let updtpop = resetNeuronOfPop (population network) (Just n) t
    let newnetwork = Network updtpop (connections network)
    put (newnetwork, spk)
    return spk

-- | Apply a presynaptic spike to a neuron at time t
applyPreSynapticSpike :: (Int, Double) -> SynInfo -> State NetworkState NetworkValue
applyPreSynapticSpike spk syninfo = do
    (Network pop conn, spktrn) <- get
    let updtpop = applyPreSynapticSpikeToPop spk syninfo pop
    let newnetwork = Network updtpop conn
    put (newnetwork, spktrn)
    return spktrn

-- | NetworkValue alias - delayed spikes after tsim
type DelayedSpikes = SpikeTrain

passThroughNetwork :: Network -> SpikeTrain -> Double -> NetworkState
passThroughNetwork net spkTrain tsim =
    execState
        (go spkTrain tsim)
        (net, EmptySpikeTrain)
  where
    go EmptySpikeTrain tsim = do
        (network,spkout) <- get
        case firstSpikingNeuron
                 (population network) of
            Nothing ->
                return EmptySpikeTrain
            Just indx ->
                if (nextspktm >
                    At tsim) -- next spike time after tsim
                    then return EmptySpikeTrain
                    else do
                        resetNeuronNinNet indx tn
                        (newnet,_) <- get
                        put (newnet, concST spkout newspk)
                        go
                              (SpikeTrain
                                   (VU.singleton
                                        (Spike (indx, tn + delay))))
                              tsim
                where nextspktm =
                          nextSpikeTime
                              ((neurons . population) network V.!
                               indx)
                      tn =
                          getTime nextspktm
                      newspk =
                          SpikeTrain
                              (VU.singleton
                                   (Spike (indx, tn)))
                      delay =
                          1.0 -- Spike transmission delay hardcoded
    go (SpikeTrain spktrn) tsim = do
        let Spike (indx,t) =
                VU.head spktrn -- First spike
        if (t <= tsim)
            then do
                --update network to before the spike arrives and collect any delayed
                --spikes
                dspktrn <-
                    go EmptySpikeTrain t -- Delayed spikes
                -- Apply first spike
                (net,_) <- get
                updateAllNeurons net indx t
                -- Process reminder spikes
                let restspk =
                        VU.tail spktrn -- reminder of spikes
                go (mergeST dspktrn (SpikeTrain restspk))
                   tsim
            else do
                -- Input spikes arrive after simulation time so they don't matter
                dspktrn <-
                    go EmptySpikeTrain tsim
                return (mergeST dspktrn (SpikeTrain spktrn))

-- | update all neurons connected to this axon (im very proud of this line of code :D .. i know.. silly)
updateAllNeurons :: Network -> Int -> Double -> State NetworkState ()
updateAllNeurons net indx t =
    VU.mapM_
        (\(ind,sinf) ->
              applyPreSynapticSpike
                  (ind, t)
                  sinf)
        (M.takeRow ((syninfo . connections) net) indx)
{-# INLINE updateAllNeurons #-}
