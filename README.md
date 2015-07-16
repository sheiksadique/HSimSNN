# HSimSNN
Haskell based simulator for spiking neural networks


Status
======

(WORK IN PROGRESS)

- This project is very much in its infancy.
- Basic data types are defined.
- Currently the network can spit spontaneous activity of individual disconnected neurons.


Goals
=====

    -- Event based simulation.
    
    Implementation goals
    --------------------

        (In the listed order)
        
        1) A basic spiking neuron model - Done, LIF without synapses.
        2) Populations of neurons - Done.
        3) Connections - Not yet implemented
        4) Network level simulations - Semi implemented. Only computes spontaneous activity.

Long term goals
===============

    (Not necessarily in the listed order)

    1) Synaptic plasticity
    2) Complex neuronal dynamics
    

Download and Installation
=========================

For installing the library using cabal

```
$ git clone https://github.com/sheiksadique/HSimSNN.git
$ cd HSimSNN
$ cabal install
```

For documentation

```
$ cd HSimSNN
$ cabal haddock --executables
$ <yourfavourate_browser> ./dist/doc/html/HSimSNN/index.html
```
