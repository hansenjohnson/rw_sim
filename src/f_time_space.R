## f_time_space ##
# figure of time-space series for each behaviour

library(tidyverse)

# process -----------------------------------------------------------------

load('runs/medium/traveling.rda')
tr_d = df %>% 
  filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Traveling')
rm(df)

load('runs/medium/feeding.rda')
fd_d = df %>% 
  filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Feeding')
rm(df)

load('runs/medium/socializing.rda')
sc_d = df %>% 
  filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Socializing')
rm(df)

# combine
df = rbind(fd_d,tr_d,sc_d)

# specify breaks
brks = c(0.00001,0.0001,0.001,0.01,0.1)

# define factors for plot order
df$bh = factor(df$bh, levels = c('Traveling', 'Feeding', 'Socializing'), ordered = TRUE)
df$platform = factor(df$platform, labels = c('Acoustic', 'Visual'))

# plot --------------------------------------------------------------------

# plot
plt = ggplot(df, aes(x=x, y=y,z = stat(count)))+
  geom_bin2d(aes(fill = stat(density)), binwidth = c(5,5)) +
  scale_fill_viridis_c(trans = "log",breaks = brks, labels = brks)+
  labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Probability')+
  facet_grid(bh+platform~t)+
  coord_equal()+
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0.5,0), "cm"))+
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))

# save
ggsave(plt, filename = 'figures/f_time_space.png', width = 12, height = 10, units = 'in', dpi = 300)
