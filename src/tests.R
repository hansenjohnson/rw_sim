## tests ##

source('src/functions.R')
library(tidyverse)

# single whale ------------------------------------------------------------

# process

tst = rw_sim(hrs = 48, bh = 'random', nt = 300)

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


# acoustic detection function ---------------------------------------------

L = 1
x0 = 10
k = -0.5

# detection function
xx = seq(0,25,0.1)
yy = detection_function(x = xx, L = L, x0 = x0, k = k)

ggplot(data.frame(xx,yy))+
  geom_path(aes(x=xx,y=yy))

tst = init_acoustic(nrws = 1e5, L = L, x0 = x0, k = k)

# histogram
ggplot(tst)+
  geom_histogram(aes(x=rngs/1e3), binwidth=1)

# 2d bin
ggplot(tst)+
  geom_bin2d(aes(x=x0/1e3,y=y0/1e3), binwidth=1)+
  coord_equal()


# parallel processing -----------------------------------------------------

system.time({
  tst = rw_sims(nrws = 1e3, bh = 'feeding', hrs = 2, platform = 'visual', run_parallel = TRUE)  
})

system.time({
  tst = rw_sims(nrws = 1e3, bh = 'feeding', hrs = 2, platform = 'visual', run_parallel = FALSE)  
})

# multiple whales ---------------------------------------------------------

# process
tst = rw_sims(nrws = 1e2, bh = 'feeding', hrs = 2, platform = 'visual', run_parallel = TRUE)

# scatterplot of initial positions
tst %>%
  filter(t == 0) %>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha=0.05)

# bin initial position
tst %>%
  filter(t == 0) %>%
  ggplot(aes(x=x,y=y))+
  geom_bin2d()+
  scale_fill_viridis_c()

# plot histogram of initial ranges
tst %>%
  filter(t == 0) %>%
  ggplot(aes(x=dfc))+
  geom_histogram()

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
tst = compare_rw_sims(nrws = 100, hrs = 1, bh = 'feeding')

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

# reading data ------------------------------------------------------------

load('data/socializing.rda')

# plot map
ggplot(df)+
  geom_point(aes(x=x, y=y, color = platform), alpha = 0.3, size = 0.3)+
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "none")

# summary figure ----------------------------------------------------------

summary_plot(df = df, bh = 'socializing')

# distance figure ----------------------------------------------------------

distance_plot(df = tst, bh = 'feeding')

# movie -------------------------------------------------------------------

# process
tst = compare_rw_sims(nrws = 100, hrs = 12,nt = 60, bh = 'feeding')

# process movie
make_movie(df = df, bh = 'test1', movie_speed = 7)

# movie2 ------------------------------------------------------------------

# load('data/socializing.rda')

df = compare_rw_sims(nrws = 100, hrs = 96, bh = 'feeding')

# process movie
make_movie2(df = df, bh = 'test2', movie_speed = 7)

# master ------------------------------------------------------------------

proc_rw_sim(nrws = 100, nt = 3600, bh = 'feeding')

# density plot ------------------------------------------------------------

load('data/feeding.rda')

# process
# df = rw_sims(nrws = 100, bh = 'feeding', hrs = 48, radius = 0.1e3)

df_i = filter(df,t == 24)

# plot map
ggplot(df_i, aes(x=x, y=y))+
  stat_density_2d(aes(x=x, y=y, fill = ..level..), geom = "polygon")+
  # geom_point(aes(group = id), size = 0.2, alpha = 0.1)+
  # stat_density_2d()+
  scale_fill_viridis_c()+
  # geom_bin2d() +
  # geom_hex() +
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()
  # theme(legend.position = "none")

# distance quantiles ------------------------------------------------------

# df = compare_rw_sims(nrws = 100, a_radius = 15e3, v_radius = 0.1e3, bh = 'feeding')
load('data/feeding.rda')

dq = bin_quantile(df)

ggplot(dq)+
  geom_ribbon(aes(x=t, ymin = q25, ymax = q75, fill=platform), color = NA, alpha = 0.3)+
  geom_path(aes(x=t, y=q50, color=platform), alpha = 1, size = 1)+
  labs(x = 'Time [hr]', y = 'Distance from center [km]', color = '50%:', fill = '25-75%')+
  theme_bw()+
  theme(legend.position = 'bottom')


# quantile map ------------------------------------------------------------

load('data/feeding.rda')

df_i = filter(df,t==12)

# coord for circles
cir = combine_circle_quantiles(df_i, probs = c(0.25,0.5,0.75))

# plot maps
ggplot()+
  geom_point(data = df_i, aes(x=x, y=y), size = 0.2, alpha = 0.5)+
  geom_path(data = cir, aes(x=x,y=y,group = id, color = platform))+
  coord_fixed()+
  facet_wrap(~platform)+
  labs(x = 'Easting [km]', y = 'Northing [km]', color = 'Quantile')+
  theme_bw()
