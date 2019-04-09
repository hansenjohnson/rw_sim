## f_example_tracks ##
# figure of example tracks for each movement behaviour

# user input --------------------------------------------------------------

# number of hours to simulate
nhrs = 24

# size of timestep [seconds]
nt = 60

# setup -------------------------------------------------------------------

library(tidyverse)
library(grid)
source('src/functions.R')

# reproducible results
set.seed(110)

# process -----------------------------------------------------------------

# simulate
tr = rw_sim(hrs = nhrs, bh = 'traveling', nt = nt) %>%
  mutate(
    x = x/1e3,
    y = y/1e3,
    r = r/1e3,
    bh = 'Traveling' 
  )
fd = rw_sim(hrs = nhrs, bh = 'feeding', nt = nt) %>%
  mutate(
    x = x/1e3,
    y = y/1e3,
    r = r/1e3,
    bh = 'Feeding' 
  )
sc = rw_sim(hrs = nhrs, bh = 'socializing', nt = nt) %>%
  mutate(
    x = x/1e3,
    y = y/1e3,
    r = r/1e3,
    bh = 'Socializing'
  )

# definitions -------------------------------------------------------------

# plot main map
lim = ceiling(tr$r)

# define offsets for tracks
fdi = mutate(fd, x=x+10, y=y+10)
sci = mutate(sc, x=x+20, y=y-10)

# define offset for rectangles
off = 2

# colors
tr_col = 'black'
fd_col = 'blue'
sc_col = 'red'

# plot --------------------------------------------------------------------

# main plot
p1 = ggplot()+
  # paths
  geom_path(data = tr,aes(x=x,y=y), alpha = 0.5, color = tr_col)+
  geom_path(data = fdi,aes(x=x,y=y), alpha = 0.5, color = fd_col)+
  geom_path(data = sci,aes(x=x,y=y), alpha = 0.5, color = sc_col)+
  
  # boxes
  geom_rect(data = fdi, aes(ymin = min(y)-off, ymax = max(y)+off,
                            xmin = min(x)-off, xmax = max(x)+off),
            fill = NA, linetype = 2, color = "darkslategrey") + 
  geom_rect(data = sci, aes(ymin = min(y)-off, ymax = max(y)+off,
                            xmin = min(x)-off, xmax = max(x)+off),
            fill = NA, linetype = 2, color = "darkslategrey") + 
  
  # labels
  geom_text(aes(x=23, y=-2, label = 'Socializing'), color = sc_col)+
  geom_text(aes(x=14, y=28, label = 'Feeding'), color = fd_col)+
  geom_text(aes(x=-20, y=-8, label = 'Traveling'), color = tr_col)+
  
  # formatting
  coord_equal(xlim = c(-lim,lim), ylim = c(-lim,lim))+
  labs(x = 'Easting [km]', y = 'Northing [km]', color = NULL, shape = NULL)+
  theme_bw()+
  theme(legend.position = "none")

# inset 1
p2 = ggplot()+
  geom_path(data = fd,aes(x=x,y=y), alpha = 0.5, color = fd_col)+
  coord_equal()+
  labs(x = NULL, y = NULL, color = NULL, shape = NULL)+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position = "none")

# inset 2
p3 = ggplot()+
  geom_path(data = sc,aes(x=x,y=y), alpha = 0.5, color = sc_col)+
  coord_equal()+
  labs(x = NULL, y = NULL, color = NULL, shape = NULL)+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position = "none")

# define layout matrix
# m = rbind(c(1,1,1,1,4,4),
#           c(1,1,1,1,2,2),
#           c(1,1,1,1,2,2),
#           c(1,1,1,1,3,3),
#           c(1,1,1,1,3,3),
#           c(1,1,1,1,5,5))

m = rbind(c(1,1,1,1,2,2),
          c(1,1,1,1,2,2),
          c(1,1,1,1,3,3),
          c(1,1,1,1,3,3))

# save
png(filename = 'figures/f_example_tracks.png', width = 9, height = 7, units = 'in', res = 300)
grid.arrange(p1,p2,p3,layout_matrix = m)
dev.off()
