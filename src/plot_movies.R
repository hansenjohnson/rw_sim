## plot_movies ##

source('src/functions.R')

bhv = c('linear', 'traveling', 'feeding', 'socializing','random')

runs = list.dirs(path = 'runs/', recursive = FALSE, full.names = TRUE)

for(ii in seq_along(runs)){
  run = runs[ii]
  
  message('Plotting movies from data in: ', run)
  
  for(jj in seq_along(bhv)){
    
    # read in data
    load(paste0(run, '/data/', bhv[jj], '.rda'))
    
    # plot movie
    make_movie(df = df, bh = bhv[jj], movie_speed = 7, fig_dir = paste0(run, '/figures/'))
  }
  
}




