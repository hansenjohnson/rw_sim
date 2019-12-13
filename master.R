## rw_sim ##
# Simulate right whale movement to compare visual and acoustic survey results

source('src/functions.R')

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

# figures
source('src/f_example_tracks.R')
source('src/f_detection_function.R')
source('src/f_time_space.R')
source('src/f_timeseries.R')
source('src/f_range_probability.R')
source('src/f_difference.R')
