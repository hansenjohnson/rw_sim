## f_tide ##
# figure of tidal component used in model

library(tidyverse)
library(grid)
source('src/functions.R')

# get data ----------------------------------------------------------------

longitude = -63
latitude = 48
t0 = as.POSIXct('2018-06-01', tz='UTC')
hrs = 96
dt = 2.5

# create posix time vector
tp = seq.POSIXt(from = t0, to = t0+hrs*60*60, by = dt)

# make webtide prediction
tmp = webtide("predict", longitude=longitude, latitude=latitude,time = tp, plot = FALSE)

# assemble data
td = tibble(
  time = tmp$time,
  z = tmp$elevation,
  u = tmp$u,
  du = cumsum(tmp$u*dt)
)

p1 = ggplot(td)+
  geom_path(aes(x = time, y = z))+
  labs(x = NULL, y = 'Elev. [m]')+
  theme_bw()+
  theme(axis.text.x=element_blank())

p2 = ggplot(td)+
  geom_path(aes(x = time, y = u))+
  labs(x = NULL, y = 'Horz. vel. [m/s]')+
  theme_bw()+
  theme(axis.text.x=element_blank())

p3 = ggplot(td)+
  geom_path(aes(x = time, y = du))+
  labs(x = NULL, y = 'Horz. disp. [m]')+
  theme_bw()

# combine and save
png(filename = 'figures/f_tide.png', width = 7, height = 5, units = 'in', res = 300)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
dev.off()