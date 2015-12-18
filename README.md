# HSimSNN
Haskell based simulator for spiking neural networks


Status
======

HSimSNN is fully functional spiking neural network simulator.

- Basic networks of LIF neurons can be implemented.

Goals
=====

Design goals
------------

Event based simulation.

Implementation goals
--------------------

(In the listed order)

- A basic spiking neuron model - Done.
- Populations of neurons - Done.
- Connections - Done.
- Network level simulations - Done
- User defined neuron models/dynamics.

Long term goals
===============

(Not necessarily in the listed order)

- Synaptic dynamics
- Custom neuron models
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

The data is saved to txt files that are then used to generate plots in python
using matplotlib



Change log
==========

(0.2.0.0)
---------

- Added transmission delays to spikes. This ensure that the simulation doesn't
explode and progresses over time (esp for recurrent netwoks)

- Added refractory period to spikes. Once agian as a measure to ensure the
  network activity doesn't explode.
