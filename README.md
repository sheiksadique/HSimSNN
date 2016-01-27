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

(Recommended) Install with stack
Make sure your stack is up to date.

(Optional)
```
$ stack upgrade
```


```
$ git clone https://github.com/sheiksadique/HSimSNN.git
$ cd HSimSNN
$ stack build
```


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

To run the file in ./examples/basics.hs follow the below instructions:

If you compiled the library with stack

```
$ stack runhaskell examples/basics.hs
```


If you compiled/installed the library with cabal

```
$ runhaskell examples/basics.hs
```

The data is saved to text (.txt) files that are then used to generate plots in Python
using matplotlib. 

WARNING: If you do not have python or matplotlib installed the figure
will not be generated.



Change log
==========

(0.2.0.0)
---------

- Added transmission delays to spikes. This ensure that the simulation doesn't
explode and progresses over time (esp for recurrent networks)

- Added refractory period to spikes. Once again as a measure to ensure the
  network activity doesn't explode.
