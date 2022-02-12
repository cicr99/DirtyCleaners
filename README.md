# DirtyCleaners
Simulation project using Haskell

## Description
You'll be able to find a full project description and solution of the problem in [here](report.md). There is also a section with some experiments on simulations and its results.

## Requirements
This project was developed using Haskell Platform. You can access [this link](https://www.cyberithub.com/how-to-install-haskell-platform-on-ubuntu-20-04-lts/) and follow its steps for installation.

## Usage
From the root of the project, you can run the following command on the terminal to run the execution of the simulations:

```bash
runhaskell main.hs
```

This command will allow you to select the parameters of the execution:
- Number of rows
- Number of columns
- Number of turns in which the board will change randomly
- Number of obstacles
- Number of children (No need to provide the amount of corrals because it is the same number as the children)
- Number of robots
- The type of robot agent you would like to run (got to the description for a better understanding of these strategies):
  - 1 for a reactive robot
  - 2 for a proactive robot
- Percentage of dirt in the initial board
- The maximum amount of steps you would like the simulation to run
- The number of simulations you would like to run

After filling out these fields the program will executes the said simulations and youl get a message similar to this one:
```
Simulation Results:
Total simulations where the robots couldn't keep the house clean: 29
Total simulations where the robots managed to keep the children from generating dirt: 0
Average dirt percentage at the end: 61.490654
```

You could also use the ghc's interactive mode by typing `ghci` on your terminal. After that you should run `:l main` to load all the functionalities of the project. After that you can call any method and watch the output.