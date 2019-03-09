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
  bhs = c('linear', 'traveling', 'feeding', 'socializing', 'random'),
  tide_lat = 48,
  tide_lon = -63,
  tide_t0 = as.POSIXct('2018-06-01', tz='UTC'),
  L = 1,
  x0 = 5,
  k = -0.7,
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
  bhs = c('linear', 'traveling', 'feeding', 'socializing', 'random'),
  tide_lat = 48,
  tide_lon = -63,
  tide_t0 = as.POSIXct('2018-06-01', tz='UTC'),
  L = 1,
  x0 = 10,
  k = -0.6,
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
  bhs = c('linear', 'traveling', 'feeding', 'socializing', 'random'),
  tide_lat = 48,
  tide_lon = -63,
  tide_t0 = as.POSIXct('2018-06-01', tz='UTC'),
  L = 1,
  x0 = 20,
  k = -0.3,
  visual_radius = 100,
  run_parallel = TRUE
)
