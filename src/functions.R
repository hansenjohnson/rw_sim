## functions ##
# source code for rw_sim analysis

# setup -------------------------------------------------------------------

# libraries
suppressPackageStartupMessages(library(oce))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(overlapping))
suppressPackageStartupMessages(library(raster))

# functions ---------------------------------------------------------------

deg2rad = function(deg){
  # convert degrees to radians
  rad = deg*pi/180
  return(rad)
}

rad2deg = function(rad){
  # convert radians to degrees
  deg = rad*180/pi
  return(deg)
}

rw_sim = function(
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'feeding', # behaviour (feeding, traveling, socializing)
  nt = 60,        # new time resolution after subsampling [sec]
  sub = TRUE      # subsample data to new rate, nt
){
  # simulate right whale movement with vectorized correlated random walk
  # and optional one-dimensional tidal advection
  
  # define turn rate (deg / 10m)
  if(bh == 'feeding'){
    tr = 19.3  
  } else if(bh == 'traveling'){
    tr = 5.3   
  } else if(bh == 'socializing'){
    tr = 52.5
  } else if(bh == 'random'){
    tr = 360
  } else if(bh == 'linear'){
    tr = 0
  } else {
    return('Behaviour not recognized!')
  }
  
  # create time vector
  t = seq(from = 0, to = hrs*60*60, by = dt)
  
  # length time vector
  n = length(t)
  
  # create blank tide vector
  if(is.null(td)){
    td = rep(0, n)
  }
  
  # calculate speeds
  spd = runif(min = 0, max = 1.23, n = n)
  
  # calculate travel distances
  dst = spd*dt
  dpt = cumsum(dst)
  
  # calculate turn angles
  max_ang = dst*deg2rad(tr)/10
  ang = runif(min = -max_ang, max = max_ang, n=n-1)
  
  # choose starting angle and add to rest
  ang = cumsum(c(runif(n = 1, min = 0, max = 2*pi), ang))
  
  # wrap turning angles
  ang = (ang + (2*pi)) %% (2*pi)
  
  # y movement
  y = c(y0, y0+cumsum(dst*sin(ang)))
  
  # x movement
  x = c(x0, x0+cumsum(dst*cos(ang)))
  
  # combine into data frame
  df = tibble(x = x[1:n], y = y[1:n], t, ang = rad2deg(ang), spd, dst, dpt)
  
  # downsample data
  if(sub){
    df = df[seq(from = 1, to = nrow(df), by = round(nt/dt)),]
  }
  
  # calculate range from center
  df$r = sqrt(df$x^2 + df$y^2)
  
  # add behaviour
  df$bh = bh
  
  return(df)
}

detection_function = function(x,L=1.045,x0=10,k=-0.3){
  # Construct an acoustic detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}

init_acoustic = function(nrws=1e3,L=1.045,x0=10,k=-0.3,max_radius=x0*2.5){
  # initialize field of simulated whales with acoustic detection function
  
  # initialize values
  cnt = 0
  RNG = XX = YY = rep(NA, nrws)
  
  # determine starting points
  while(cnt <= nrws){
    
    # choose range
    r = max_radius*sqrt(runif(n = 1, min = 0, max = 1))
    
    # choose to include or not
    tmp = rbinom(1, size = 1, prob = detection_function(x=r,L=L,x0=x0,k=k))
    
    # add to list
    if(tmp == 1){
      
      # choose angle
      a = 2*pi*runif(n = 1, min = 0, max = 1)
      
      # convert to xy
      x=r*cos(a)
      y=r*sin(a)
      
      # store
      XX[cnt] = round(x,3)
      YY[cnt] = round(y,3)
      cnt = cnt+1
    }
  }
  
  # return data
  out = data.frame(x=XX*1e3,y=YY*1e3)
  return(out)
}

init_visual = function(nrws=1e3, radius = 1e2){
  # initialize field of simulated whales with visual detection function
  
  # calculate angles and ranges
  a = 2*pi*runif(n = nrws, min = 0, max = 1)
  r = radius*sqrt(runif(n = nrws, min = 0, max = 1))
  
  # convert to xy
  x=r*cos(a)
  y=r*sin(a)
  
  # return data
  out = data.frame(x=round(x),y=round(y))
}

rw_sims = function(nrws = 1e2,
                   hrs = 48, 
                   bh = 'feeding', 
                   nt = 300, 
                   L = 1.045,
                   x0 = 10,
                   k = -0.3,
                   radius = 1e2,
                   run_parallel = TRUE){
  # simulate movements of whale field initialized with given detection function
  
  # startup message
  message('\n## NARW MOVEMENT MODEL RUN ##\n')
  message('Input parameters:')
  message('   number of whales: ', nrws)
  message('   number of hours: ', hrs)
  message('   time resolution [sec]: ', nt)
  message('   number of timesteps: ', hrs*60*60/nt)
  message('   movement type: ', bh)
  message('Simulating whale movements...')
  
  # initialize data values
  nseq = seq(from = 1, to = nrws, by = 1)
  tic = Sys.time()
  
  # initial starting positions
  ini = data.frame(x = rep(0, nrws), y = rep(0, nrws))
  iaco = init_acoustic(nrws = nrws, L = L, x0 = x0, k = k)
  ivis = init_visual(nrws = nrws, radius = radius)
  
  if(run_parallel){
    # determine number of cores available
    numCores = detectCores()
    
    message('Running in parallel with ', numCores, ' cores...')
    
    # model movements
    DF = mclapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt)
    }, mc.cores = numCores)
    
  } else {
    
    # model movements
    DF = lapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt)
    })
    
  }
  
  # shift tracks to starting position based on detection functions
  for(ii in 1:nrws){
    iv = DF[[ii]] %>%
      mutate(
        x=x+ivis$x[ii],
        y=y+ivis$y[ii],
        platform = 'visual'
      )
    ia = DF[[ii]] %>%
      mutate(
        x=x+iaco$x[ii],
        y=y+iaco$y[ii],
        platform = 'acoustic'
      )
    DF[[ii]] = bind_rows(iv,ia)
  }
  
  # flatten list to combine all whales
  df = bind_rows(DF, .id = 'id')
  
  # update ranges
  df$r = sqrt(df$x^2 + df$y^2)
  
  # convert time to hours
  df$t = df$t/60/60

  # convert distance to kilometers
  df$x = df$x/1e3
  df$y = df$y/1e3
  df$r = df$r/1e3
  df$dst = df$dst/1e3
  df$dpt = df$dpt/1e3
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  message('Done! Time elapsed: ', format(toc))
  
  return(df)
}

run_rw_sim = function(
  run_dir = 'tests/master_run',
  nrws = 1e2,
  hrs = 24,
  nt = 3600,
  bhs = c('linear', 'traveling', 'feeding', 'socializing', 'random'),
  L = 1,
  x0 = 10,
  k = -0.5,
  radius = 100,
  run_parallel = TRUE
){
  
  # create dir
  if(!dir.exists(run_dir)){dir.create(run_dir, recursive = TRUE)}
  
  message('\n##############################')
  message('## NARW MOVEMENT SIMULATION ##')
  message('##############################\n')
  message('\nWriting all data to: ', run_dir, '\n')
  
  # record start time
  tic = Sys.time()
  
  ## simulate whale movement ##
  DF = vector('list', length(bhs))
  for(ii in seq_along(bhs)){
    bh = bhs[ii]
    
    # run movement simulations
    df = rw_sims(nrws=nrws,hrs=hrs,bh=bh,nt=nt,L=L,x0=x0,k=k,
                 radius=radius,run_parallel=run_parallel)
    
    # save run data
    save(df,run_dir,nrws,hrs,nt,bh,L,x0,k,radius,run_parallel,
         file = paste0(run_dir, '/', bh, '.rda'))
    
    # clear memory
    rm(df)
  }
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  
  message('\n#############################')
  message('## SIMULATION COMPLETE :)  ##')
  message('#############################\n')
  message('Time elapsed: ', format(toc))
}
