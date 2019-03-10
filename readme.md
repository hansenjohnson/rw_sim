# rw_sim
Estimating uncertainty in right whale location following visual or acoustic detection

## Project structure

`master.R` - master script (with simulation parameters) that executes entire analysis
`src/` - all source code for analysis, including functions (`functions.R`) and plot code (`plot_*.R`)
`runs/` - data and figures from each model run
`cache` - cached data used across runs
`reports/` - presentations and reports

## Running the job remotely

Move to the remote machine, make sure `src/` and `master.R` are up to date. In most cases it will be necessary to used cached tidal data (unless `WebTide` is configured on the remote machine). Then use the following line to start the job:
```
nohup Rscript -e "source('master.R')" &
```
The job is now running and all that would appear in the console is written to `nohup.out`. Try to remember to record the PID, so the process can be killed later if needed. You can verify the model is running using:
```
cat nohup.out
```
