## f_time_space ##
# figure of time-space series for each behaviour

library(tidyverse)
library(rnaturalearth)
library(sf)

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

# get basemap data
bg = ne_countries(scale = "medium", continent = 'north america', returnclass = "sf") %>%
  filter(name %in% c('Canada', 'United States'))

# project map data
bg_t = st_transform(bg, crs = "+proj=merc +lon_0=-50 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")

# plot --------------------------------------------------------------------

# plot uncertainty
p1 = ggplot(df, aes(x=x, y=y,z = stat(count)))+
  geom_bin2d(aes(fill = stat(density)), binwidth = c(5,5)) +
  scale_fill_viridis_c(trans = "log",breaks = brks, labels = brks)+
  labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Probability')+
  facet_grid(bh+platform~t)+
  coord_equal()+
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0.5,0), "cm"),
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 20))

# plot map
p2 = ggplot()+
  geom_sf(data = bg_t, color = 'darkslategrey', fill = 'grey')+
  coord_sf(datum = st_crs(bg_t), xlim = c(-3000,1250), ylim = c(1750,6500), expand = FALSE)+
  theme_void()

# overlay plot on map
p3 = p2+
  annotation_custom(grob=ggplotGrob(p1), ymin = 1700, ymax=5000, xmin=-2600, xmax = 1300)

# # overlay lines on plot to test scaling
# x0 = 145; y0 = 4620 # top right
# # x0 = 320; y0 = 2225 # bottom right
# p3 = p3+
#   geom_path(aes(x=c(x0,x0), y=c(y0-200,y0+200)))+
#   geom_path(aes(x=c(x0-200,x0+200), y=c(y0,y0)))

# save
ggsave(p3, filename = 'figures/f_time_space.jpg', width = 7.5, height = 8.4, units = 'in', dpi = 600)
