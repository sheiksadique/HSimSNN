-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes
import Simulation.HSimSNN.Neuron

import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad.State

-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: Population, connections:: Connections}
               deriving Show


-- | value of network state
type NetworkValue = SpikeTrain
-- | network state
type NetworkState = (Network, SpikeTrain)

-- | Pass a set of spikes through a network and get the network state and spikes
--
-- The basic steps of simulation/transformation are
--
-- - Check for the smallest 'timeOfNextSpike' spike in the 'population'
--
-- - Compare this to the first spike of SpikeTrain
--      
--      - Apply which ever spike occors first to the network through 'connections'
--
--      - Append any output spikes to the response 'SpikeTrain'
--
-- - Repeat the same process again.
-- - Stop when
--
--      - input 'SpikeTrain' is empty 
--
--      - and all 'timeOfNextSpike' in the network are 'Never'
--
--      - Or when time is larger than some limit
--
-- - Return the final 'SpikeTrain' and 'Network'
passThroughNetwork :: SpikeTrain -> Double -> State NetworkState NetworkValue
passThroughNetwork EmptySpikeTrain tsim = do
    (network, spkout) <- get
    let i = firstSpikingNeuron (population network)
    if (i==Nothing) 
        then return spkout
    else do
        let indx = fromJust i
        let nextspktm = nextSpikeTime $ ((neurons.population) network)!! indx
        if (nextspktm>At tsim) 
            then return spkout-- next spike time after tsim 
        else do
            let newspk = SpikeTrain $V.fromList [(indx,getTime nextspktm)]
            resetNeuronNinNet indx (getTime nextspktm)
            (newnet,_) <- get
            put ( newnet, 
                  concST spkout newspk )
            passThroughNetwork newspk tsim
            
-- applies a SpikeTrain to the network
passThroughNetwork (SpikeTrain spktrn) tsim = do
    let (indx,t) = V.head spktrn
    let restspk = V.tail spktrn
    if (t<=tsim) then do
        (net, _) <- get
        --update network to before the spike arrives
        passThroughNetwork EmptySpikeTrain t
        --update all neurons connected to this axon
        mapM (\(ind,sinf) -> applyPreSynapticSpike (ind,t) sinf) (((syninfo.connections) net)!!indx)
        -- process the remaining spikes
        if (V.length restspk == 0) then
            passThroughNetwork EmptySpikeTrain tsim
        else
            passThroughNetwork (SpikeTrain restspk) tsim
    else
        passThroughNetwork EmptySpikeTrain tsim




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
