module Simulation.HSimSNN.Connections where

import Simulation.HSimSNN.Population

data Connections = Connections {srcPop:: Population, dstPop:: Population, weight::Double}
                   deriving Show
