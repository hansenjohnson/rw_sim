## rw_sim ##
# Simulate right whale movement to compare visual and acoustic survey results

# inputs ------------------------------------------------------------------

# number of right whales to simulate
nrws = 1e5

# number of hours to simulate
nhrs = 96

# size of timestep [seconds]
nt = 60*60

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

# feeding
proc_rw_sim(nrws = nrws, hrs = nhrs, nt = nt, bh = 'feeding')

# socializing
proc_rw_sim(nrws = nrws, hrs = nhrs, nt = nt, bh = 'socializing')

# traveling
proc_rw_sim(nrws = nrws, hrs = nhrs, nt = nt, bh = 'traveling')

# linear
proc_rw_sim(nrws = nrws, hrs = nhrs, nt = nt, bh = 'linear')

# random
proc_rw_sim(nrws = nrws, hrs = nhrs, nt = nt, bh = 'random')

# summary plots -----------------------------------------------------------

source('src/plot_detection_function.R')
source('src/plot_tide.R')
source('src/plot_example_tracks.R')
source('src/plot_movies.R')
source('src/plot_time_space.R')
source('src/plot_timeseries.R')
source('src/plot_range_probability.R')
