## plot_movies ##

source('src/functions.R')

bhv = c('linear', 'traveling', 'feeding', 'socializing', 'random')
# bhv = c('random')
for(ii in seq_along(bhv)){
  load(paste0('data/', bhv[ii], '.rda'))
  
  make_movie_q(df = df, bh = bhv[ii])
}
