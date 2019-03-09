# rw_sim
Simulate right whale movement to compare visual and acoustic survey results

## Project structure

`master.R` - master script (with simulation parameters) that executes entire analysis
`src/` - all source code for analysis, including functions (`functions.R`) and plot code (`plot_*.R`)
`src/functions.R` - functions called by master script  
`src/tests.R` - diagnostics to test major functions  
`runs/` - data and figures from each model run
`cache` - cached data (currently just tide data)
`reports/` - presentations and reports
`wrk/` - development sandbox  

## Running the job remotely (on kaos)
The movement simulations are computationally intensive. Processing speed can be dramatically improved by running the simulations in parallel on a multi-core machine. Parallel processing increases speed by about a factor of two on my 4 core machine. More cores should provide more speed (to a point). `kaos` is a 24 core linux machine at Dal that should provide some extra speed. At first glance using `kaos` appears to increase speed by another factor of 2 relative to parallelized runs on my laptop.

1. SSH into anchor (use dal password at prompt)
```
ssh anchor
```
2. SSH into kaos (use dal password at prompt)
```
ssh kaos.phys.ocean.dal.ca
```
3. Move to project directory
```
cd Projects/rw_sim/
```
4. Run analysis in background
```
nohup Rscript -e "source('master.R')" &
```
The job is now running and all that would appear in the console is written to `nohup.out`. Try to remember to record the PID, so the process can be killed later if needed.
5. Verify model is running
```
cat nohup.out
```
6. Close terminal, and wait for the job to finish!
