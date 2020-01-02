## rw_sim ##
# Simulate right whale movement to compare visual and acoustic survey results

# setup -------------------------------------------------------------------

# load external functions
source('src/functions.R')

# define figure directory
fig_dir = 'figures'

# run 1: short range ------------------------------------------------------

run_rw_sim(
  run_dir = 'runs/short',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  L = 1.13,
  x0 = 5,
  k = -0.4,
  radius = 100,
  run_parallel = TRUE
)

# run 2: medium range ------------------------------------------------------

run_rw_sim(
  run_dir = 'runs/medium',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  L = 1.045,
  x0 = 10,
  k = -0.3,
  radius = 100,
  run_parallel = TRUE
)

# run 3: long range ------------------------------------------------------

run_rw_sim(
  run_dir = 'runs/long',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  L = 1.02,
  x0 = 15,
  k = -0.25,
  radius = 100,
  run_parallel = TRUE
)

# visualize ---------------------------------------------------------------

# create figure directory
if(!dir.exists(fig_dir)){dir.create(fig_dir, recursive = TRUE)}

# generate figures
source('src/f_example_tracks.R')
source('src/f_detection_function.R')
source('src/f_time_space.R')
source('src/f_timeseries.R')
source('src/f_range_probability.R')
source('src/f_difference.R')
