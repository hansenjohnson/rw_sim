## f_movie_summary ##
# Plot a movie comparing runs for presentations

# input -------------------------------------------------------------------

# figure directory
fig_dir = 'figures/'

# movie info
movie_speed = 20

# setup -------------------------------------------------------------------

library(tidyverse)

# process -----------------------------------------------------------------

load('runs/medium/traveling.rda')
tr_d = df %>% 
  # filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Traveling')
rm(df)

load('runs/medium/feeding.rda')
fd_d = df %>% 
  # filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Feeding')
rm(df)

load('runs/medium/socializing.rda')
sc_d = df %>% 
  # filter(t %in% c(0,6,12,24,48,96)) %>%
  mutate(bh = 'Socializing')
rm(df)

# combine
df = rbind(fd_d,tr_d,sc_d)

# definitions
brks = c(0.00001,0.0001,0.001,0.01,0.1,1)

# define factors for plot order
df$bh = factor(df$bh, levels = c('Traveling', 'Feeding', 'Socializing'), ordered = TRUE)
df$platform = factor(df$platform, labels = c('Acoustic', 'Visual'))

# plot --------------------------------------------------------------------

message('\nMaking movie plots...')

# find data limits
dmax = ceiling(max(df$r,na.rm = TRUE))
tmax = ceiling(max(df$t,na.rm = TRUE))

# make plot directory
plt_dir = paste0(fig_dir, '/movie/')
if(!dir.exists(plt_dir)){dir.create(plt_dir, recursive = TRUE)}

# initialize plot variables
tvec = seq(from = 0, to = tmax, by = 1)
pb = txtProgressBar(min = 1, max = length(tvec), style = 3)

# create movie plots
for(it in seq_along(tvec)){
  
  # subset
  df_i = filter(df, t == tvec[it])
  # mn_i = filter(mn, t <= tvec[it])
  
  # plot maps
  p1 = ggplot()+
    geom_bin2d(data = df_i, aes(x=x, y=y, fill = stat(density)), binwidth = c(5,5)) +
    scale_fill_viridis_c(trans = "log", breaks = brks, labels = brks, limits=range(brks))+
    lims(x = c(-dmax,dmax), y = c(-dmax,dmax))+
    facet_grid(platform~bh)+
    labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Probability')+
    theme_bw()+
    coord_equal()+
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))+
    ggtitle(paste0('Elapsed time = ', tvec[it], ' hrs'))
  
  # save plots
  ggsave(filename = paste0(plt_dir, 'hr_', sprintf("%02d",tvec[it]), '.png'),
         plot = p1, width = 7,height = 4,dpi = 300)
  
  # update progress bar
  setTxtProgressBar(pb, it)
}

message('\nConverting plots to movie...\n')

# write system command to create giff
cmd = paste0('ffmpeg -framerate ', movie_speed,' -i ', plt_dir, 'hr_%02d.png -pix_fmt yuv420p -y figures/movie.mp4')

# execute system command
system(cmd)

message('\nDone!')