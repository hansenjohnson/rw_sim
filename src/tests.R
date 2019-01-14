## tests ##

source('src/functions.R')
library(tidyverse)

# single whale ------------------------------------------------------------

# process
tst = rw_sim(hrs = 48, bh = 'feeding', nt = 300)

# plot xy movement
ggplot(tst)+
  geom_path(aes(x, y))+
  coord_equal()

# plot directions
ggplot(tst)+
  geom_path(aes(x = t, y = ang), size = 0.5, alpha = 0.7)

# plot speeds over time
ggplot(tst)+
  geom_path(aes(x = t, y = spd), size = 0.5, alpha = 0.7)

# plot step distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dst), size = 0.5, alpha = 0.7)

# plot along-path distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dpt))

# plot distance from start over time
ggplot(tst)+
  geom_path(aes(x = t, y = dfc))

# multiple whales ---------------------------------------------------------

# process
tst = rw_sims(nrws = 100, bh = 'feeding', hrs = 48, radius = 0.1e3)

# plot map
ggplot(tst)+
  geom_path(aes(x, y, group = id), size = 0.2, alpha = 0.1)+
  coord_equal()

# plot distance from center over time
ggplot(tst)+
  geom_path(aes(x = t, y = dfc, group = id), size = 0.2, alpha = 0.1)

# plot direction over time
ggplot(tst)+
  geom_path(aes(x = t, y = ang, group = id), size = 0.2, alpha = 0.1)

# plot speed over time
ggplot(tst)+
  geom_path(aes(x = t, y = spd, group = id), size = 0.2, alpha = 0.1)

# plot step distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dst, group = id), size = 0.2, alpha = 0.1)

# plot path distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dpt, group = id), size = 0.2, alpha = 0.1)

# plot step distance histogram
ggplot(tst)+
  geom_histogram(aes(x = dst), binwidth = 5)

# plot path distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dpt, group = id), size = 0.2, alpha = 0.1)

# plot speed histogram
ggplot(tst)+
  geom_histogram(aes(x = spd), binwidth = 0.01)

# plot direction histogram
ggplot(tst)+
  geom_histogram(aes(x = ang), binwidth = 1)

# platform comparison -----------------------------------------------------

# process
tst = compare_rw_sims(nrws = 100, a_radius = 15e3, v_radius = 0.1e3, bh = 'feeding')

# plot map
ggplot(tst)+
  geom_path(aes(x=x, y=y, group=id, color=platform), alpha = 0.3, size = 0.3)+
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "none")

# plot distance from center over time
ggplot(tst)+
  geom_path(aes(x=t, y=dfc, group=id, color=platform), alpha = 0.3)+
  facet_grid(~platform)+
  theme_bw()+
  theme(legend.position = "none")

# plot direction over time
ggplot(tst)+
  geom_path(aes(x=t, y=ang, group=id, color=platform), alpha = 0.1)+
  facet_grid(~platform)+
  theme_bw()+
  theme(legend.position = "none")

# plot speed over time
ggplot(tst)+
  geom_path(aes(x=t, y=spd, group=id, color=platform), alpha = 0.01)+
  facet_grid(~platform)+
  theme_bw()+
  theme(legend.position = "none")

# plot path distance over time
ggplot(tst)+
  geom_path(aes(x=t, y=dpt, group=id, color=platform), alpha = 0.01)+
  facet_grid(~platform)+
  theme_bw()+
  theme(legend.position = "none")

# summary figure ----------------------------------------------------------

summary_plot(df = tst, bh = 'socializing')

# distance figure ----------------------------------------------------------

distance_plot(df = tst, bh = 'feeding')

# movie -------------------------------------------------------------------

# process
tst = compare_rw_sims(nrws = 100, a_radius = 15e3, v_radius = 0.1e3, bh = 'feeding')

# process movie
make_movie(df = tst, bh = 'feeding', movie_speed = 7)

# reading data ------------------------------------------------------------

load('data/socializing.rda')

# plot map
ggplot(df)+
  geom_point(aes(x=x, y=y, color = platform), alpha = 0.3, size = 0.3)+
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "none")

# master ------------------------------------------------------------------

proc_rw_sim(nrws = 100, a_radius = 15e3, v_radius = 0.1e3, nt = 3600, bh = 'feeding')
