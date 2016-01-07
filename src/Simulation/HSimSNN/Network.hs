-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes
import Simulation.HSimSNN.Neuron
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad.State


-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: !Population, connections:: !Connections}
               deriving Show


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


passThroughNetwork :: SpikeTrain -> Double -> State NetworkState DelayedSpikes
passThroughNetwork EmptySpikeTrain tsim = do
    (network,spkout) <- get
    let i =
            firstSpikingNeuron
                (population network) -- Check for any spikes
    if (i == Nothing) -- No spikes
        then return EmptySpikeTrain
        else do
            let indx =
                    fromJust i
            let nextspktm =
                    nextSpikeTime $
                    ((neurons . population) network) V.!
                    indx
            if (nextspktm >
                At tsim) -- next spike time after tsim
                then return EmptySpikeTrain
                else do
                    let tn =
                            getTime nextspktm
                    let newspk =
                            SpikeTrain $
                            V.fromList
                                [Spike (indx, tn)]
                    resetNeuronNinNet indx tn
                    (newnet,_) <- get
                    put (newnet, concST spkout newspk)
                    passThroughNetwork
                        (SpikeTrain $
                         V.fromList
                             [Spike (indx, tn + 1.0)])
                        tsim
  where
    delay =
        1.0 -- Spike transmission delay hardcoded

passThroughNetwork (SpikeTrain spktrn) tsim = do
    let Spike (indx,t) =
            V.head spktrn -- First spike
    if (t <= tsim)
        then do
            --update network to before the spike arrives and collect any delayed
            --spikes
            dspktrn <-
                passThroughNetwork EmptySpikeTrain t -- Delayed spikes
            -- Apply first spike
            (net,_) <- get
            --update all neurons connected to this axon (im very proud of this line of code :D .. i know.. silly)
            mapM_
                (\(ind,sinf) ->
                      applyPreSynapticSpike
                          (ind, t)
                          sinf)
                (((syninfo . connections) net) !!
                 indx)
            -- Process reminder spikes
            let restspk =
                    V.tail spktrn -- reminder of spikes
            if isEmptySpikeTrain dspktrn
                then if isEmptySpikeTrain
                            (SpikeTrain restspk)
                         then passThroughNetwork EmptySpikeTrain tsim
                         else passThroughNetwork
                                  (SpikeTrain restspk)
                                  tsim
                else if isEmptySpikeTrain
                            (SpikeTrain restspk)
                         then passThroughNetwork dspktrn tsim -- >>= return
                         else passThroughNetwork
                                  (mergeST dspktrn (SpikeTrain restspk))
                                  tsim
        else do
            -- Input spikes arrive after simulation time so they don't matter
            dspktrn <-
                passThroughNetwork EmptySpikeTrain tsim
            return (mergeST dspktrn (SpikeTrain spktrn))
