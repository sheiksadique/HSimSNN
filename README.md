# HSimSNN
Haskell based simulator for spiking neural networks


Status
======

(WORK IN PROGRESS)

- This project is very much in its infancy and doesn't do much yet.
- Basic data types are defined. Functionality still missing.


Goals
=====

    -- Event based simulation ? (Perhaps this should be a primary design
      criterion right from the start?) 
    
    Implementation goals
    --------------------

        (In the listed order)
        
        1) A basic spiking neuron model
        2) Populations of neurons
        3) Connections
        4) Network level simulations

Long term goals
===============

    (Not necessarily in the listed order)

    1) Synaptic plasticity
    

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
