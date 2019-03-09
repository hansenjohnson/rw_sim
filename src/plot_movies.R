## plot_movies ##

source('src/functions.R')

bhv = c('traveling', 'feeding', 'socializing','random', 'linear')
for(ii in seq_along(bhv)){
  load(paste0('data/', bhv[ii], '.rda'))
  
  make_movie(df = df, bh = bhv[ii])
}
