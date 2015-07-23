# HSimSNN
Haskell based simulator for spiking neural networks


Status
======

(WORK IN PROGRESS)

- This project is very much in its infancy.
- Basic data types are defined.
- Currently a spike list can be applied to the network to readout its spiking activity, including any spontaneous spikes from it.
- Connectivity missing


Goals
=====

Design goals
------------

Event based simulation.

Implementation goals
--------------------

(In the listed order)

- A basic spiking neuron model - Done, LIF without synapses.
- Populations of neurons - Done.
- Connections - Not yet implemented
- Network level simulations - Done

Long term goals
===============

(Not necessarily in the listed order)

- Synaptic plasticity
- Complex neuronal dynamics
    

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

Examples
========

You can try some example scripts that demonstrate the use of the library.


```
$ runhaskell examples/basics.hs
```

