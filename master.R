## rw_sim ##
# Simulate right whale movement to compare visual and acoustic survey results

source('src/functions.R')

# run 1: short range ------------------------------------------------------

run_rw_sim(
  name = 'short',
  description = 'short detection range',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  tide_lat = NA,
  tide_lon = NA,
  tide_t0 = NA,
  L = 1.13,
  x0 = 5,
  k = -0.4,
  visual_radius = 100,
  run_parallel = TRUE
)

# run 2: medium range ------------------------------------------------------

run_rw_sim(
  name = 'medium',
  description = 'medium detection range',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  tide_lat = NA,
  tide_lon = NA,
  tide_t0 = NA,
  L = 1.045,
  x0 = 10,
  k = -0.3,
  visual_radius = 100,
  run_parallel = TRUE
)

# run 3: long range ------------------------------------------------------

run_rw_sim(
  name = 'long',
  description = 'long detection range',
  nrws = 1e5,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  tide_lat = NA,
  tide_lon = NA,
  tide_t0 = NA,
  L = 1.02,
  x0 = 15,
  k = -0.25,
  visual_radius = 100,
  run_parallel = TRUE
)

# plot figures ------------------------------------------------------------

source('src/f_example_tracks.R')
source('src/f_detection_function.R')
source('src/f_time_space.R')
source('src/f_timeseries.R')
source('src/f_range_probability.R')
source('src/f_movies.R')
