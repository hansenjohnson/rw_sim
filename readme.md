# rw_sim
Simulate right whale movement to compare visual and acoustic survey results

## TO DO
* Quantify the timestep at which the distributions are statistically identical for each movement case
  * Figure out which statistical test to use for comparing distributions
* Plot as probability surface rather than raw points
* Consider a state-space model where animat can choose to transition between movement behaviours

## Background
Visual surveys report the position of a sighted whale to perhaps within 10 to 100 meters of its actual location. Acoustic surveys only report that a detected whale was within a given listening radius of the monitoring platform. Other work suggests the detection radius is commonly on the order 15 kilometers.

The large uncertainty in the absolute position of an acoustically detected whale has been cited as reason that acoustic detections cannot be used to inform management decisions. That reasoning omits an important consideration: whale movement. There is always some latency period between when a whale is cited and a management action is taken, during which time the observed whale may move. It varies widely depending on the observation platform and management measure, but the time delay from sightings to management is typically about 24 hours. Whales can move great distances in this period of time. There must be a point in time at which the uncertainty that arises from movement equals the initial uncertainty due to detection method. At this point, both detection methods are effectively providing the same management information. If this time occurs within the 24 hour response time, one could argue that acoustic and visual observations provide exactly the same management information.

The goal of this project is to simulate right whale movements to quantitatively compare the time evolution of the uncertainty in whale position that arises from visual and acoustic detection methods.

## Methods

The right whale movement model is an auto-correlated random walk based on the model used by van der Hoop et al (2012) to quantify ship strike risk in Roseway Basin, and by Vanderlaan et al (2018) to estimate gear encounter rates in the southern Gulf of St Lawrence. The two random terms in the model are speed and direction. Speed is selected at each timestep from a uniform distribution between 0 and 1.23 m/s. The initial travel direction is selected from a uniform distribution from 1 to 360 degrees. The turning rate is then constrained to a given angle per 10 meters of travel distance. I will use 3 different parameterizations of turning rate based on observations from Mayo and Marx (1990) to approximate movement patterns associated with feeding (22.1 deg / 10m), socializing (50 deg / 10m), and traveling (5 deg / 10m). I will also model the two extreme cases of entirely linear (0 deg / 10m) and entirely random (360 deg / 10m) travel to help with model interpretation.

The model framework requires an extremely high time resolution to constrain turning rate appropriately. I will run the model at a 2.5 second resolution, then downsample to 5 minute resolution to allow for fast computation while retaining the integrity of the simulated track.

I will randomly seed an area of a given radius with 10000 simulated whales and track their movements over a period of 48 hours. I will repeat this for each movement model with starting radii of 0.1 and 15 km to represent initial uncertainties from visual and acoustic surveys, respectively.

## Project structure

`rw_sim.R` - master script (with simulation parameters) that executes entire analysis (simulates, plots in `figures/` and saves in `data/`)  
`src/functions.R` - functions called by master script  
`src/tests.R` - diagnostics to test major functions  
`figures` - plots and movies of simulation results  
`data` - output from simulations  
`wrk` - development sandbox  
