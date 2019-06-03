## tests ##

source('src/functions.R')

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
  geom_path(aes(x = t, y = r))

# multiple whales ---------------------------------------------------------

# process
tst = rw_sims(bh = 'feeding', hrs = 24, run_parallel = FALSE)

# scatterplot of initial positions
tst %>%
  filter(t == 0) %>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  coord_equal()+
  facet_wrap(~platform)

# plot histogram of initial ranges
tst %>%
  filter(t == 0) %>%
  ggplot(aes(x=r))+
  geom_histogram()+
  facet_wrap(~platform)

# plot map
ggplot(tst)+
  geom_path(aes(x, y, group = id), size = 0.2, alpha = 0.1)+
  coord_equal()+
  facet_wrap(~platform)

# plot distance from center over time
ggplot(tst)+
  geom_path(aes(x = t, y = r, group = id), size = 0.2, alpha = 0.1)+
  facet_wrap(~platform)

# plot direction over time
ggplot(tst)+
  geom_path(aes(x = t, y = ang, group = id), size = 0.2, alpha = 0.1)+
  facet_wrap(~platform)

# plot speed over time
ggplot(tst)+
  geom_path(aes(x = t, y = spd, group = id), size = 0.2, alpha = 0.1)+
  facet_wrap(~platform)

# plot step distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dst, group = id), size = 0.2, alpha = 0.1)+
  facet_wrap(~platform)

# plot path distance over time
ggplot(tst)+
  geom_path(aes(x = t, y = dpt, group = id), size = 0.2, alpha = 0.1)+
  facet_wrap(~platform)

# plot speed histogram
ggplot(tst)+
  geom_histogram(aes(x = spd), binwidth = 0.01)+
  facet_wrap(~platform)

# plot direction histogram
ggplot(tst)+
  geom_histogram(aes(x = ang), binwidth = 1)+
  facet_wrap(~platform)

# parallel processing -----------------------------------------------------

system.time({
  tst = rw_sims(nrws = 1e3, bh = 'feeding', hrs = 2, platform = 'visual', run_parallel = TRUE)  
})

system.time({
  tst = rw_sims(nrws = 1e3, bh = 'feeding', hrs = 2, platform = 'visual', run_parallel = FALSE)  
})

# master ------------------------------------------------------------------

run_rw_sim(
  run_dir = 'tests/master/',
  nrws = 1e3,
  hrs = 96,
  nt = 3600,
  bhs = c('traveling', 'feeding', 'socializing'),
  tide = NA,
  L = 1,
  x0 = 10,
  k = -0.5,
  radius = 100,
  run_parallel = TRUE
)

# COMPARISON PLOT ---------------------------------------------------------

# visual - acoustic range over time (maybe separate figure plot?)

# reading data ------------------------------------------------------------

load('tests/master/feeding.rda')

# plot map comparing IDs between platforms
df %>%
  mutate(id = as.numeric(id)) %>%
  filter(t < 12 & id < 6) %>%
  mutate(id = factor(id,ordered=T)) %>%
  ggplot()+
  geom_path(aes(x=x, y=y, color = id, group = id))+
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()

# plot map comparing platforms
df %>%
  filter(t <= 12) %>%
  ggplot()+
  geom_path(aes(x=x, y=y, color = t, group = id), size = 0.3, alpha = 0.3)+
  scale_color_viridis_c()+
  facet_grid(~platform)+
  coord_fixed()+
  theme_bw()
