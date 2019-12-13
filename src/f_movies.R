## f_movies ##
# generate movies to illustrate evolution of uncertainty

# input -------------------------------------------------------------------

# list run names
rns = c('medium')

# list behaviours
bhs = c('traveling','feeding','socializing')

# figure directory
fig_dir = 'figures/'

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

# make movies
cnt = 1
N = length(rns)*length(bhs)
for(ii in seq_along(rns)){
  irun = rns[ii]
  for(jj in seq_along(bhs)){
    ibhs = bhs[jj]
    ifile = paste0('runs/',irun,'/',ibhs,'.rda')
    
    # read in data
    message('Processing file ', cnt, ' of ', N, ':\n', ifile)
    
    # read in data
    message('   Loading...')
    load(ifile)
    
    # calculate quantile distances
    message('   Making movie...')
    make_movie(df = df, bh = ibhs, movie_speed = 7, fig_dir = fig_dir)
    
    rm(df)
    cnt = cnt+1
    message('\nDone!')
  }
}