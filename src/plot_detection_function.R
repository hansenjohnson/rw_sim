## plot_detection_function ##

library(tidyverse)
library(gridExtra)
source('src/functions.R')

# read in data
load('data/traveling.rda')

# select initial timestep
df_i = filter(df, t == 0 & platform == 'acoustic')

# logistic regression -----------------------------------------------------

# range vector
xx = seq(from = 0, to = 25, by = 0.1) # range vector
yy = detection_function(x = xx, x0 = 10)

# plot --------------------------------------------------------------------

# plot detection function
p1 = ggplot(data.frame(xx,yy))+
  geom_path(aes(x=xx,y=yy))+
  labs(x = 'Range [km]', y = 'Probability of detection')+
  ylim(c(0,1))+
  xlim(c(0,25))+
  theme_bw()
  
# plot range histogram
p2 = ggplot(df_i)+
  geom_histogram(aes(x = dfc), color = 'black', fill = 'grey', binwidth = 1)+
  labs(x = 'Range [km]', y = 'Count', subtitle = paste0('Total: ', nrow(df_i)))+
  xlim(c(0,25))+
  theme_bw()

# plot starting range
brks = c(1,10,100,1000,10000)

p3 = ggplot(df_i, aes(x=x, y=y))+
  geom_bin2d(aes(fill = stat(count)), binwidth=1) +
  scale_fill_viridis_c(trans = "log", breaks = brks, labels = brks, limits = range(brks))+
  labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Count')+
  xlim(c(-25,25))+
  ylim(c(-25,25))+
  coord_equal()+
  theme_bw()+
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))

# combine and save
m = rbind(c(1,1,1,1,3,3,3,3),
          c(1,1,1,1,3,3,3,3),
          c(2,2,2,2,3,3,3,3),
          c(2,2,2,2,3,3,3,3))

png(filename = 'figures/detection_function.png', width = 10, height = 6, units = 'in', res = 250)
grid.arrange(p1,p2,p3, layout_matrix = m)
dev.off()
