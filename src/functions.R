## functions ##
# source code for rw_sim analysis

# setup -------------------------------------------------------------------

# libraries
suppressPackageStartupMessages(library(oce))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(overlapping))
suppressPackageStartupMessages(library(sf))
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

get_tide = function(longitude = -63, 
                    latitude = 48, 
                    t0 = as.POSIXct('2018-06-01', tz='UTC'),
                    hrs = 96,
                    dt = 2.5,
                    cache_dir = 'cache/tide/'){
  # get tidal horizontal velocity at time and location using local WebTide program
  
  # create cache dir
  if(!dir.exists(cache_dir)){dir.create(cache_dir, recursive = TRUE)}
  
  # define out file
  ofile = paste0(cache_dir, 'TIDE_', format(t0, '%Y-%m-%d_%H%M%S'), 
                 '_', longitude, '_', latitude, '_', hrs, 'hrs.rda')
  
  if(!file.exists(ofile)){
    # create posix time vector
    tp = seq.POSIXt(from = t0, to = t0+hrs*60*60, by = dt)
    
    # make webtide prediction
    td = webtide("predict", longitude=longitude, latitude=latitude,time = tp, plot = FALSE)
    
    # save
    save(td, file = ofile)
  } else {
    message('Using cached tide data in: ', ofile)
    load(ofile)
  }
  
  return(td$u)
}

rw_sim = function(
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'feeding', # behaviour (feeding, traveling, socializing)
  nt = 60,        # new time resolution after subsampling [sec]
  td = NULL,      # optional vector of horizontal tidal velocities [m/sec]
  sub = TRUE      # subsample data to new rate, nt
){
  # simulate right whale movement with vectorized correlated random walk
  # and optional one-dimensional tidal advection
  
  # define turn rate (deg / 10m)
  if(bh == 'feeding'){
    tr = 22.1   
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
  y = c(y0, y0+cumsum(dst*sin(ang)+td*dt))
  
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
  
  return(df)
}

detection_function = function(x,L=1,x0=10,k=-0.5){
  # Construct an acoustic detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}

init_acoustic = function(nrws=1e3,L=1,x0=10,k=-0.5,max_radius=x0*2.5){
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
      XX[cnt] = x
      YY[cnt] = y
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

rw_sims = function(ini = data.frame(x0=runif(1e2,1,10),y0=runif(1e2,1,10)),
                   hrs = 48, 
                   bh = 'feeding', 
                   platform = 'acoustic', 
                   nt = 300, 
                   td = NULL,
                   run_parallel = TRUE){
  # simulate movements of whale field initialized with given detection function
  
  # determine number of whales
  nrws = nrow(ini)
  
  # message
  message('\n## NARW MOVEMENT MODEL RUN ##\n')
  message('Input parameters:')
  message('   number of whales: ', nrws)
  message('   number of hours: ', hrs)
  message('   time resolution [sec]: ', nt)
  message('   number of timesteps: ', hrs*60*60/nt)
  message('   platform type: ', platform)
  message('   movement type: ', bh)
  message('Simulating whale movements...')
  
  # initialize data values
  nseq = seq(from = 1, to = nrws, by = 1)
  tic = Sys.time()
  
  if(run_parallel){
    # determine number of cores available
    numCores = detectCores()
    
    message('Running in parallel with ', numCores, ' cores...')
    
    DF = mclapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt,td=td)
    }, mc.cores = numCores)
  } else {
    DF = lapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt,td=td)
    })
  }
  
  # flatten list to combine all whales
  df = bind_rows(DF, .id = 'id')
  
  # add platform
  df$platform = platform
  
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

calc_distance_m = function(df){
  # compute mean and sd of distance at each timestep
  mn = df %>%
    group_by(t, platform) %>%
    summarize(
      sv = sd(r),
      r = mean(r)
    )
  
  return(mn)
}

calc_distance_q = function(df,lower=0.25,upper=0.75){
  # compute distance quantiles at each timestep
  dq = df %>%
    group_by(t, platform) %>%
    summarize(
      lwr = quantile(r,lower),
      med = quantile(r,0.5),
      upr = quantile(r,upper)
    )
  
  return(dq)
}

calc_overlap = function(df, hrs = seq(from = 0,to = 96,by = 1)){
  
  ovl = rep(0, length(hrs))
  for(ll in seq_along(hrs)){
    
    # isolate hr
    df_i = filter(df, t == hrs[ll])
    
    # pull range
    a = df_i %>% filter(platform == 'acoustic') %>% pull(r)
    v = df_i %>% filter(platform == 'visual') %>% pull(r)
    
    # calc overlap
    dl = list(acoustic = a, visual = v)
    ovl[ll] = overlap(dl, plot = FALSE)$OV
  }
  
  out = data.frame(hrs, ovl)
  return(out)
}

calc_ks = function(df, hrs = seq(from = 0,to = 96,by = 1)){
  
  ks = rep(0, length(hrs))
  for(ll in seq_along(hrs)){
    
    # isolate hr
    df_i = filter(df, t == hrs[ll])
    
    # pull range
    a = df_i %>% filter(platform == 'acoustic') %>% pull(r)
    v = df_i %>% filter(platform == 'visual') %>% pull(r)
    
    # calc ks test
    ks[ll] = ks.test(x = a, y = v, alternative = "two.sided")$p.value
  }
  
  out = data.frame(hrs, ks)
  return(out)
}

calc_acoustic_residuals = function(df, hrs = seq(from = 0,to = 96, by = 1), max_r = 200, grid_res = 5, n_tot = 1e5){
  
  # build template raster
  template = raster(xmn = -max_r, xmx = max_r, ymn = -max_r, ymx = max_r, resolution = c(grid_res,grid_res))
  
  ar = rep(0, length(hrs))
  pb = txtProgressBar(min = 1, max = length(hrs), style = 3)
  for(ii in seq_along(hrs)){
    
    # isolate hr
    df_i = filter(df, t == hrs[ii])
    
    # split visual and acoustic data
    a_df = df_i %>% filter(platform == 'acoustic') %>% transmute(x=x,y=y)
    v_df = df_i %>% filter(platform == 'visual') %>% transmute(x=x,y=y)
    
    # build presence raster
    a_presence = rasterize(a_df, template, field = 1, background = 0)
    v_presence = rasterize(v_df, template, field = 1, background = 0)
    av_presence = a_presence - v_presence
    
    # build count raster
    a_count = rasterize(a_df, template, field = 1, fun = "count")
    
    # calculate acoustic residual (count of av_presence)
    a_residual = a_count*av_presence
    
    # calculate proportion of residual
    ar[ii] = cellStats(a_residual, stat = 'sum')/n_tot
    
    # update progress bar
    setTxtProgressBar(pb, ii)
  }
  
  out = data.frame(hrs, ar)
  return(out)
}

make_movie = function(df, bh, movie_speed = 7, fig_dir){
  # plot movie (requires ffmpeg utility)
  
  message('\nMaking movie plots...')
  
  # input parameters
  movie_name = paste0(fig_dir, '/', bh,'.mp4')
  
  # calculate mean and standard deviations
  mn = calc_distance_m(df)
  
  # find data limits
  dmax = ceiling(max(df$r,na.rm = TRUE))
  tmax = ceiling(max(df$t,na.rm = TRUE))
  smax = ceiling(max(mn$r+mn$sv,na.rm = TRUE))
  
  # make plot directory
  plt_dir = paste0(fig_dir, bh, '/')
  if(!dir.exists(plt_dir)){dir.create(plt_dir, recursive = TRUE)}
  
  # initialize plot variables
  tvec = seq(from = 0, to = tmax, by = 1)
  pb = txtProgressBar(min = 1, max = length(tvec), style = 3)
  
  # create movie plots
  for(it in seq_along(tvec)){
    
    # subset
    df_i = filter(df, t == tvec[it])
    mn_i = filter(mn, t <= tvec[it])
    
    # pull for overlap calc
    a = df_i %>% filter(platform == 'acoustic') %>% pull(r)
    v = df_i %>% filter(platform == 'visual') %>% pull(r)
    
    # calculate overlap
    dl = list(acoustic = a, visual = v)
    ovl = overlap(dl, plot = FALSE)$OV
    
    # breaks for colors
    brks = c(1,10,1e2,1e3,1e4,1e5)
    
    # plot maps
    p1 = ggplot()+
      geom_bin2d(data = df_i, aes(x=x, y=y, fill = stat(count)), binwidth = 1) +
      scale_fill_viridis_c(trans = "log", breaks = brks, 
                           labels = brks, limits=range(brks))+
      lims(x = c(-dmax,dmax), y = c(-dmax,dmax))+
      facet_wrap(~platform)+
      labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Count')+
      theme_bw()+
      coord_equal()+
      guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))+
      ggtitle(paste0('Elapsed time = ', tvec[it], ' hrs'))
    
    # plot distributions
    p2 = ggplot(df_i)+
      geom_density(aes(x=r, color = platform, fill = platform), alpha = 0.2)+
      lims(x = c(0,dmax))+
      labs(x = 'Range [km]', y = 'Probability Density')+
      theme_bw()+
      annotate(geom = "text", x=Inf, y = Inf, vjust=1.5, hjust=1,
               label = paste0('Overlap: ', round(ovl*100), '%'))+
      theme(legend.position = 'none')
    
    # plot mean and sd
    p3 = ggplot(mn_i)+
      geom_ribbon(aes(x=t, ymin = r-sv, ymax = r+sv, fill=platform), 
                  color = NA, alpha = 0.3)+
      geom_path(aes(x=t, y=r, color=platform), alpha = 1, size = 1)+
      xlim(c(0,tmax))+
      ylim(c(0,smax))+
      labs(x = 'Time [hr]', y = 'Range [km]', color = 'Platform', 
           fill = 'Platform')+
      theme_bw()
    
    # make plot layout
    lay = rbind(c(1,1,1),
                c(1,1,1),
                c(2,3,3))
    
    # layer plots
    png(filename = paste0(plt_dir, 'hr_', sprintf("%02d",tvec[it]), '.png'), 
        height = 8, width = 9, units = 'in', res = 250)
    grid.arrange(p1,p2,p3,layout_matrix = lay)
    dev.off()
    
    # update progress bar
    setTxtProgressBar(pb, it)
  }
  
  message('\nConverting plots to movie...\n')
  
  # write system command to create giff
  cmd = paste0('ffmpeg -framerate ', movie_speed,' -i ', plt_dir, 'hr_%02d.png -pix_fmt yuv420p -y ', movie_name)
  
  # execute system command
  system(cmd)
  
  message('\nDone! Movie saved as: ', movie_name)
}

plot_init_acoustic = function(ini, L, x0, k, fig_dir){
  # plot initial acoustic positions
  
  # convert to km
  ini$x = ini$x/1e3
  ini$y = ini$y/1e3
  
  # calculate ranges
  ini$r = round(sqrt(ini$x^2+ini$y^2),2)
  
  # determine max range
  rmax = ceiling(max(ini$r))
  
  # range vector
  xx = seq(from = 0, to = rmax, by = 0.1) # range vector
  yy = detection_function(x=xx, x0=x0, L=L, k=k)
  
  # plot detection function
  p1 = ggplot(data.frame(xx,yy))+
    geom_path(aes(x=xx,y=yy))+
    labs(x = 'Range [km]', y = 'Probability of detection')+
    ylim(c(0,1))+
    xlim(c(0,rmax))+
    theme_bw()
  
  # plot range histogram
  p2 = ggplot(ini)+
    geom_histogram(aes(x = r), color = 'black', fill = 'grey', binwidth = 1)+
    labs(x = 'Range [km]', y = 'Count', subtitle = paste0('Total: ', nrow(ini)))+
    xlim(c(0,rmax))+
    theme_bw()
  
  p3 = ggplot(ini, aes(x=x, y=y))+
    geom_bin2d(aes(fill = stat(count)), binwidth=1) +
    scale_fill_viridis_c()+
    labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Count')+
    xlim(c(-rmax,rmax))+
    ylim(c(-rmax,rmax))+
    coord_equal()+
    theme_bw()+
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))
  
  # combine and save
  m = rbind(c(1,1,1,1,3,3,3,3),
            c(1,1,1,1,3,3,3,3),
            c(2,2,2,2,3,3,3,3),
            c(2,2,2,2,3,3,3,3))
  
  # save plot
  png(filename = paste0(fig_dir, 'initial_acoustic.png'), 
      width = 10, height = 6, units = 'in', res = 300)
  grid.arrange(p1,p2,p3, layout_matrix = m)
  dev.off()
}

plot_init_visual = function(ini, visual_radius, fig_dir){
  # plot initial visual positions
  
  # calculate ranges
  ini$r = round(sqrt(ini$x^2+ini$y^2),2)
  
  # range vector
  xx = seq(from = 0, to = visual_radius, length.out = 10) # range vector
  yy = rep(1, 10)
  
  # plot detection function
  p1 = ggplot(data.frame(xx,yy))+
    geom_path(aes(x=xx,y=yy))+
    labs(x = 'Range [m]', y = 'Probability of detection')+
    ylim(c(0,1))+
    xlim(c(0,visual_radius))+
    theme_bw()
  
  # plot range histogram
  p2 = ggplot(ini)+
    geom_histogram(aes(x = r), color = 'black', fill = 'grey', bins = 30)+
    labs(x = 'Range [m]', y = 'Count', subtitle = paste0('Total: ', nrow(ini)))+
    xlim(c(0,visual_radius))+
    theme_bw()
  
  p3 = ggplot(ini, aes(x=x, y=y))+
    geom_bin2d(aes(fill = stat(count)), binwidth=10) +
    scale_fill_viridis_c()+
    labs(x = 'Easting [m]', y = 'Northing [m]', fill = 'Count')+
    xlim(c(-visual_radius,visual_radius))+
    ylim(c(-visual_radius,visual_radius))+
    coord_equal()+
    theme_bw()+
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))
  
  # combine and save
  m = rbind(c(1,1,1,1,3,3,3,3),
            c(1,1,1,1,3,3,3,3),
            c(2,2,2,2,3,3,3,3),
            c(2,2,2,2,3,3,3,3))
  
  # save plot
  png(filename = paste0(fig_dir, 'initial_visual.png'), 
      width = 10, height = 6, units = 'in', res = 300)
  grid.arrange(p1,p2,p3, layout_matrix = m)
  dev.off()
}

plot_range_probability = function(bhs = c('Traveling','Feeding','Socializing'), 
                                  data_dir, 
                                  fig_dir){
  # plot probability of location with a range radius given behaviour and time
  
  # extract probabilities
  DQ = vector('list', length(bhs))
  for(ii in seq_along(bhs)){
    
    ibh = bhs[ii]
    load(paste0(data_dir,tolower(ibh),'.rda'))
    
    # compute p
    DQ[[ii]] = df %>%
      group_by(t, platform) %>%
      summarize(
        p5 = length(r[r<=5])/length(r),
        p10 = length(r[r<=10])/length(r),
        p15 = length(r[r<=15])/length(r),
        p25 = length(r[r<=25])/length(r)
      ) %>%
      gather(key = dist, value = p, p5:p25) %>%
      mutate(
        dist = factor(dist, levels = c('p5','p10','p15','p25'), ordered = TRUE),
        bh = ibh
      )
  }
  
  # combine
  dq = bind_rows(DQ)
  
  # rename factor levels for plotting
  levels(dq$dist) = c('5km', '10km', '15km', '25km')
  dq$bh = factor(dq$bh, levels = bhs, ordered = TRUE)
  
  # plot
  p1 = ggplot(dq)+
    geom_path(aes(x=t,y=p,group=platform,color=platform))+
    ylim(c(0,1))+
    facet_grid(bh~dist)+
    labs(x = 'Time [hr]', y = 'Probability', color = NULL)+
    theme_bw()+
    theme(legend.position = "bottom", legend.direction = 'horizontal')
  
  # save
  ggsave(plot = p1, filename = paste0(fig_dir, 'range_probability.png'), 
         width = 8, height = 6, dpi = 300)
  
}

plot_timeseries = function(bhs = c('Traveling','Feeding','Socializing'), 
                           data_dir, fig_dir){
  # plot stacked time series of mean and standard deviation of ranges
  
  # loop through behaviours and process
  DQ = vector('list', length(bhs))
  for(ii in seq_along(bhs)){
    
    ibh = bhs[ii]
    load(paste0(data_dir, '/', tolower(ibh),'.rda'))
    
    idq = calc_distance_m(df)
    idq$bh = ibh
    
    DQ[[ii]] = idq
  }
  
  # combine
  dq = bind_rows(DQ)
  
  # arrange factors
  dq$bh = factor(dq$bh, levels = bhs, ordered = TRUE)
  
  # plot
  p1 = ggplot(dq)+
    geom_ribbon(aes(x=t, ymin = r-sv, ymax = r+sv, fill=platform), 
                color = NA, alpha = 0.3)+
    geom_path(aes(x=t, y=r, color=platform), alpha = 1, size = 1)+
    labs(x = 'Time [hr]', y = 'Range [km]', fill = NULL, color = NULL)+
    theme_bw()+
    facet_wrap(~bh, ncol = 1, scales = "free_y")+
    theme(legend.position = "bottom", legend.direction = 'horizontal')
  
  # save
  ggsave(plot = p1,filename = paste0(fig_dir, 'timeseries.png'), width = 5, 
         height = 7, dpi = 300)
}

run_rw_sim = function(
  name = 'test',
  description = '',
  nrws = 1e2,
  hrs = 24,
  nt = 3600,
  bhs = c('linear', 'traveling', 'feeding', 'socializing', 'random'),
  tide_lat = 48,
  tide_lon = -63,
  tide_t0 = as.POSIXct('2018-06-01', tz='UTC'),
  L = 1,
  x0 = 10,
  k = -0.5,
  visual_radius = 100,
  run_parallel = TRUE
){
  
  # define directories
  run_dir = paste0('runs/', name, '/')
  fig_dir = paste0(run_dir, 'figures/')
  data_dir = paste0(run_dir, 'data/')
  
  # create dirs
  if(!dir.exists(fig_dir)){dir.create(fig_dir, recursive = TRUE)}
  if(!dir.exists(data_dir)){dir.create(data_dir, recursive = TRUE)}
  
  message('\n##############################')
  message('## NARW MOVEMENT SIMULATION ##')
  message('##############################\n')
  
  # record start time
  tic = Sys.time()
  
  ## get tide data ##
  
  if(is.na(tide_lat)|is.na(tide_lon)|is.na(tide_t0)){
    td = NA  
    message('Omitting tidal current')
  } else {
    td = get_tide(longitude=tide_lon,latitude=tide_lat,t0=tide_t0,hrs=hrs)  
  }
  
  ## initialize whale field ##
  
  ini_aco = init_acoustic(nrws=nrws,L=L,x0=x0,k=k)
  ini_vis = init_visual(nrws=nrws,radius=visual_radius)
  
  ## simulate whale movement ##
  
  DF = vector('list', length(bhs))
  for(ii in seq_along(bhs)){
    bh = bhs[ii]
    
    # run movement simulations
    iaco = rw_sims(ini=ini_aco,hrs=hrs,bh=bh,platform='acoustic', td = td,
                   nt=nt,run_parallel=run_parallel)
    ivis = rw_sims(ini=ini_vis,hrs=hrs,bh=bh,platform='visual',  td = td,
                   nt=nt,run_parallel=run_parallel)
    df = bind_rows(iaco,ivis)
    
    # save run data
    save(df,name,description,nrws,hrs,nt,bh,tide_lat,tide_lon,tide_t0,L,x0,
         k,visual_radius,run_parallel,file = paste0(data_dir, '/', bh, '.rda'))
    
    # clear memory
    rm(iaco,ivis,df)
  }
  
  ## plot results ##
  
  # # plot detection functions
  # plot_init_acoustic(ini = ini_aco, L = L, x0 = x0, k = k, fig_dir = fig_dir)
  # plot_init_visual(ini = ini_vis, visual_radius = visual_radius, fig_dir = fig_dir)
  # 
  # # plot timeseries
  # plot_timeseries(bhs = c('Traveling','Feeding','Socializing'), 
  #                 data_dir = data_dir, fig_dir = fig_dir)
  # 
  # # plot range probability
  # plot_range_probability(bhs = c('Traveling','Feeding','Socializing'), 
  #                        data_dir = data_dir, fig_dir = fig_dir)
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  
  message('\n#############################')
  message('## SIMULATION COMPLETE :)  ##')
  message('#############################\n')
  message('Time elapsed: ', format(toc))
}
