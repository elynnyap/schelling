Description
===========
A terminal-based simulation of Schelling's model of racial segregation. Background [here](https://lectures.quantecon.org/jl/schelling.html).

How to Run
==========
The main module for the program is Simulation.hs. There are three ways to specify the initial configuration for the model:

#1. Use a random 10x10 grid.

```
> ghc Simulation.hs
> ./Simulation -i
```

#2. Specify the dimensions and breakdown of the grid.

```
> ghc Simulation.hs
> ./Simulation -r
```

#3. Read a list of grid configuration files in .txt format.

```
> ghc Simulation.hs
> ./Simulation -f grid1.txt grid2.txt
```