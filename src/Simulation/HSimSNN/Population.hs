module Simulation.HSimSNN.Population where

import Simulation.HSimSNN.Neuron

data Population = Population [Neuron]
                  deriving Show
