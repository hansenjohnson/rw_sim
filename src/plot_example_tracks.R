## example_tracks ##

# user input --------------------------------------------------------------

# number of hours to simulate
nhrs = 24

# size of timestep [seconds]
nt = 60

# setup -------------------------------------------------------------------

library(tidyverse)
source('src/functions.R')

# reproducible results
set.seed(123)

# process -----------------------------------------------------------------

# simulate
ln = rw_sim(hrs = nhrs, bh = 'linear', nt = nt)
ln$bh = 'Linear'
tr = rw_sim(hrs = nhrs, bh = 'traveling', nt = nt)
tr$bh = 'Traveling'
fd = rw_sim(hrs = nhrs, bh = 'feeding', nt = nt)
fd$bh = 'Feeding'
sc = rw_sim(hrs = nhrs, bh = 'socializing', nt = nt)
sc$bh = 'Socializing'
rn = rw_sim(hrs = nhrs, bh = 'random', nt = nt)
rn$bh = 'Random'

# plot --------------------------------------------------------------------

# combine
df = rbind(tr,fd,sc,rn,ln)

# convert to km
df$x = df$x/1e3
df$y = df$y/1e3

# organize factors for plotting
df$bh = factor(df$bh, levels = c('Linear','Traveling','Feeding','Socializing','Random'),ordered = TRUE)

# define start and end points
t0s = filter(df,t == 0)
t0s$pos = 'Start'
t1s = filter(df,t == max(t))
t1s$pos = 'End'
pts = rbind(t0s,t1s)
lim = max(t1s$dfc)/1e3

# define plot function
plot_path = function(bhv, df=df,pts=pts){
  
  df_i = subset(df,bh==bhv)
  pts_i = subset(pts,bh==bhv)
  lim = max(df_i$dfc)/1e3
  
  pp = ggplot()+
    geom_path(data = df_i,aes(x=x,y=y), alpha = 0.5)+
    geom_point(data = pts_i,aes(x=x,y=y,shape=pos,fill=pos),size=2)+
    scale_shape_manual(values = c("Start" = 23, "End" = 24))+
    scale_fill_manual(values = c("Start" = 'lightblue', "End" = 'red'))+
    coord_equal(xlim = c(-lim,lim), ylim = c(-lim,lim))+
    labs(x = 'Easting [km]', y = 'Northing [km]', title = bhv, color = NULL, shape = NULL)+
    theme_bw()+
    theme(legend.position = "none")

  return(pp)
}

# plot
tr = plot_path(bhv = 'Traveling',df=df,pts=pts)
fd = plot_path(bhv = 'Feeding',df=df,pts=pts)
sc = plot_path(bhv = 'Socializing',df=df,pts=pts)
rn = plot_path(bhv = 'Random',df=df,pts=pts)
ln = plot_path(bhv = 'Linear',df=df,pts=pts)

# save
png(filename = 'figures/example_tracks.png', width = 10, height = 8, units = 'in', res = 250)
grid.arrange(ln,tr,fd,sc,rn, ncol=3)
dev.off()
