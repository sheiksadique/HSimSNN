-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.State
import System.Random

mkRndAxon :: Int -> Int -> [(Int, SynInfo)]
mkRndAxon n rnginit = zip [0..(n-1)] $map ((flip SynInfo) "exc") $randomRs (-1.0/(fromIntegral n),(1.0/(fromIntegral n))) $mkStdGen rnginit


main = do
    -- Get spontaneous activity till time t
    print $ evalState (passThroughNetwork EmptySpikeTrain 20.0) (network, EmptySpikeTrain)
    -- Process a list of spikes through a network
    print $ evalState (passThroughNetwork spktrn 20.0) (network, EmptySpikeTrain)

        where
            -- Population size
            npop = 10
            -- Create a random no. generator
            rng = mkStdGen 1
            -- Initialize a population of neurons with different states
            mypop = initPop [[x] | x<-(take npop (randomRs (0.0,1.0) rng))] 
            -- create connection matrix
            cm = [mkRndAxon npop rinit| rinit <-[0..(npop-1)]]
            -- Define a connection
            conn = Connections mypop cm
            -- Define a network
            network = Network mypop conn 
            -- Define an input spike train with nspk spikes
            nspk = 1000
            nindx = take nspk (randomRs (0,(npop-1)) rng)
            tindx = sort $ take nspk (randomRs (5.0,15.0) rng)
            spktrn = SpikeTrain $V.fromList $zip nindx tindx
    

