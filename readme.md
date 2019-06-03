# rw_sim
Estimating uncertainty in right whale location following visual or acoustic detection

## Project structure

`master.R` - master script (with simulation parameters) that executes entire analysis  

`src/` - all source code for analysis, including functions (`functions.R`) and scripts to produce various figures (`f_*.R`)  

`runs/` - data and figures from each model run  

`cache/` - cached data used across runs  

`reports/` - presentations and reports  

## Working remotely

### Running the job

Move to the remote machine, make sure `src/` and `master.R` are up to date. In most cases it will be necessary to used cached tidal data (unless `WebTide` is configured on the remote machine). Then use the following line to start the job:
```
nohup Rscript -e "source('master.R')" &
```
The job is now running and all that would appear in the console is written to `nohup.out`.

Record the PID here:
```
45522
```

You can verify the model is running using:
```
cat nohup.out
```

### Monitoring remote machine

Set up port forwarding:
```
ssh -L 8080:localhost:8080 kaos # on my machine
```

Then open a browser and go to http://localhost:8080/monitorix/

### Sharing displays to view figures remotely

Run ssh with a `-Y` flag to enable sharing displays, then simply open a figure (or movie) to view it within the server environment.
```
ssh -Y kaos
xdg-open /home/hansen/Projects/test.png
```

It is also possible to handle text editing this way using `gedit`

### Copy data from remote machine
Use `rsync` so that you can easily restart the transfer if it is interrupted.
```
cd /Volumes/2017_data/rw_sim/
rsync -avz hansen@kaos:Projects/rw_sim/runs .
```
