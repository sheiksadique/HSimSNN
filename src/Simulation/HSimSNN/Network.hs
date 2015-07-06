module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections

data Network = Network {population:: Population, connections:: Connections}
               deriving Show
