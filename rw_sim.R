## rw_sim ##
# Simulate right whale movement to compare visual and acoustic survey results

# inputs ------------------------------------------------------------------

# radius of acoustic detection [meters]
a_radius = 15e3

# radius of visual detection [meters]
v_radius = 0.1e3

# number of right whales to simulate
nrws = 10000

# number of hours to simulate
nhrs = 48

# size of timestep [seconds]
nt = 300

# plot movie? (requires ffmpeg)
plot_movie = TRUE

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

# feeding
proc_rw_sim(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt = nt, bh = 'feeding', plot_movie = plot_movie)

# socializing
proc_rw_sim(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt = nt, bh = 'socializing', plot_movie = plot_movie)

# traveling
proc_rw_sim(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt = nt, bh = 'traveling', plot_movie = plot_movie)

# linear
proc_rw_sim(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt = nt, bh = 'linear', plot_movie = plot_movie)

# random
proc_rw_sim(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt = nt, bh = 'random', plot_movie = plot_movie)
