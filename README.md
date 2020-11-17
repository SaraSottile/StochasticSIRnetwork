<h1>Stochastic SIR dynamics on scale-free random networks</h1>

<h2>Description</h2>

In this repository you can found the description of the model in the paper "How network properties and epidemic parameters influence stochastic SIR dynamics on scale-free random networks", written by Sara Sottile, Ozan Kahramanoğulları and Mattia Sensi. [here](http://link_arxive).

<h2>ConfigurationModel</h2>

In this repository you can produce a scale-free network via Configuration Model, choosing the exponent of the power-law. Moreover, you can decide the rate of the transmission of the epidem, the initial number of infected nodes and their position (namely hub, mean degree, peripheral, random).

<h4>Usage</h4>

To run the program, type:  
```
./benchmark [FLAG] [P]

To set the parameters, type:
N                       [number of nodes]
alpha                   [power-law exponent]
number_of_infected      [number of initial infected at the start]
end_ time               [maximum duration of the dynamics]
beta                    [disease diffusion rate]
gamma                   [disease recovery rate]
position                [position of initially infected]
```
Note that you can write as position only:
```
hub           [infected nodes are nodes of maximum degree]
peripheral    [infected nodes are nodes of minimum degree]
mean          [infected nodes are nodes of mean degree]
random        [the degree of infected nodes is choosen randomly]
```

For instance, typing

```
./configuration.py 1000 2 1 100 1 1 hub
```
will produce a network with 1000 nodes with &alpha; = 2, with 1 initial infected node on the hub, tmax = 100, &beta; = 1 and &gamma; = 1.

The program will produce two files:
1. Network file `edges.txt`, contains the list of edges.
2. Reactions file `model.txt`, contains the list of the reactions between node in order to apply afterwards `/SpecializedGillespie`.

<h2>SpecializedGillespie</h2>

SpecializedGillespie is a specialized version of the Gillespie algorithm developed to simulate the spread of an epidemic in a scale-free network built via Configuration Model. To compile te code, please follow the instruction below.


<h4>Compile</h4>

```
cd SpecializedGillespie
chmod +x compile
./compile
ls - l
```
After the simulator has been compiled, you can run the dynamics. You can find an example of model (named `model0.txt`).

<h4>Run</h4>

```
./pSSA model0.txt
```
The Configuration Model algorithm found in the folder `/ConfigurationModel` will produce a file named `model.txt` of the same type of the example.

The program will produce two files:
1. Network file `model.txt.csv`, contains the csv file of the dynamics.
2. Reactions file `traces.txt.txt`, contains the trace of the reactions at each time-step.


<h2>ProbabilityExtinction</h2>

This is the MATLAB code to find the "Probability of Extinction" described in the main manuscript. The main code is `main.m`, which reads in input a table named `table_initialstate.csv`; our table of data of Section 4.1 is provided as example of the format table needed.

<h2>Supplementary Material</h2>

In this folder you can find all the tables used in the manuscript to generate the figures.
