## functions ##
# source code for rw_sim analysis

# setup -------------------------------------------------------------------

# libraries
library(oce)
library(tidyverse)
library(gridExtra)
library(parallel)
library(overlapping)

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
                    dt = 2.5){
 
  # create posix time vector
  tp = seq.POSIXt(from = t0, to = t0+hrs*60*60, by = dt)
  
  # make webtide prediction
  td = webtide("predict", longitude=longitude, latitude=latitude,time = tp, plot = FALSE)
  
  return(td$u)
}

rw_sim = function(
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'feeding', # behaviour (feeding, traveling, socializing)
  nt = 60,         # new time resolution after subsampling [sec]
  td = NULL,
  sub = TRUE      # subsample data to new rate, nt
){
  # simulate right whale movement with vectorized correlated random walk
  # and tidal component
  
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
  
  # calculate distance from center
  df$dfc = sqrt(df$x^2 + df$y^2)
  
  return(df)
}

detection_function = function(x,L=1,x0=10,k=-0.5){
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}

init_acoustic = function(nrws=1e3,L=1,x0=20,k=-0.5){
  
  # initialize values
  cnt = 0
  RNG = XX = YY = rep(NA, nrws)
  lim = x0*2+5
  
  # determine starting points
  while(cnt <= nrws){
    
    # choose point
    ix = round(runif(n = 1, min = -lim, max = lim),1)
    iy = round(runif(n = 1, min = -lim, max = lim),1)
    irng = round(sqrt(ix^2+iy^2),1)
    
    # choose to include or not
    tmp = rbinom(1, size = 1, prob = detection_function(x=irng,L=L,x0=x0,k=k))
    
    # add to list
    if(tmp == 1){
      RNG[cnt] = irng
      XX[cnt] = ix
      YY[cnt] = iy
      cnt = cnt+1
    }
  }
  
  # return data
  out = data.frame(x0=XX*1e3, y0=YY*1e3,rngs=RNG*1e3)
  return(out)
}

init_visual = function(nrws=1e3, lim = 1e2){
  
  # calculate angles and ranges
  a = 2*pi*runif(n = nrws, min = 0, max = 1)
  rngs = lim*sqrt(runif(n = nrws, min = 0, max = 1))
  
  # convert to xy
  x0=rngs*cos(a)
  y0=rngs*sin(a)

  # return data
  out = data.frame(x0,y0,rngs)
}

rw_sims = function(nrws = 100, 
                    hrs = 48, 
                    bh = 'feeding', 
                    platform = 'acoustic', 
                    nt = 300, 
                    convert2hr = TRUE, 
                    convert2km = TRUE,
                    run_parallel = TRUE){
  # iteratively call `rw_sim` to simulate movements of many whales
  
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
  
  # get tide data
  td = get_tide(hrs = hrs)
  
  # determine initial positions
  if(platform=='acoustic'){
    ini = init_acoustic(nrws = nrws)
  } else if(platform=='visual'){
    ini = init_visual(nrws = nrws)
  }
  
  # initialize data values
  nseq = seq(from = 1, to = nrws, by = 1)
  tic = Sys.time()
  
  if(run_parallel){
    # determine number of cores available
    numCores = detectCores()
    
    message('Running in parallel with ', numCores, ' cores...')
    
    DF = mclapply(X = nseq, FUN = function(i){rw_sim(x0=ini$x0[i],y0=ini$y0[i],hrs=hrs,bh=bh,nt=nt,td=td)}, 
                  mc.cores = numCores)
  } else {
    DF = lapply(X = nseq, FUN = function(i){rw_sim(x0=ini$x0[i],y0=ini$y0[i],hrs=hrs,bh=bh,nt=nt,td=td)})
  }

  # flatten list to combine all whales
  df = bind_rows(DF, .id = 'id')
  
  # add platform
  df$platform = platform
  
  # convert time to hours
  if(convert2hr){
    df$t = df$t/60/60
  }
  
  # convert to kilometers
  if(convert2km){
    df$x = df$x/1e3
    df$y = df$y/1e3
    df$dfc = df$dfc/1e3
    df$dst = df$dst/1e3
    df$dpt = df$dpt/1e3
  }
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  message('Done! Time elapsed: ', format(toc))
  
  return(df)
}

compare_rw_sims = function(nrws = 100,
                           hrs = 48,
                           bh = 'feeding', 
                           nt = 300){
  # call `rw_sims` for two starting radii
  
  # acoustic
  df1 = rw_sims(nrws = nrws, hrs = hrs, platform = 'acoustic', bh = bh, nt=nt)
  
  # visual
  df2 = rw_sims(nrws = nrws, hrs = hrs, platform = 'visual', bh = bh, nt=nt)
  
  # combine
  df = rbind(df1,df2)
  
  return(df)
}

summary_plot = function(df, bh){
  # plot summary of data produced by `compare_rw_sims`
  
  message('\nMaking summary plot...')
  
  # plot maps
  p1 = ggplot(df)+
    geom_point(aes(x=x, y=y), size = 0.1, alpha = 0.05)+
    facet_grid(~platform)+
    labs(x = 'Easting [km]', y = 'Northing [km]')+
    coord_equal()+
    theme_bw()+
    theme(legend.position = "none")
  
  # calculate mean and standard deviations
  mn = aggregate(dfc ~ t + platform, data = df, FUN = mean)
  sv = aggregate(dfc ~ t + platform, data = df, FUN = sd)
  mn$sv = sv$dfc
  
  # plot mean and sd distances
  p2 = ggplot(mn)+
    geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), color = NA, alpha = 0.3)+
    geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
    labs(x = 'Time [hr]', y = 'Distance from center [km]', color = 'Mean:', fill = 'Standard deviation:')+
    theme_bw()+
    theme(legend.position = 'bottom')
  
  # make plot layout
  lay = rbind(c(1,1,1),
              c(1,1,1),
              c(2,2,2))
  
  # define plot name
  pname = paste0('figures/', bh, '_summary.png')
  
  # save plot
  message('\nMaking summary plot...')
  png(filename = pname, 
      height = 8, width = 9, units = 'in', res = 250)
  grid.arrange(p1,p2,layout_matrix = lay)
  dev.off() 
  
  message('Done! Summary plot saved as: ', pname)
}

calc_distance_m = function(df){
  # compute mean and sd of distance at each timestep
  mn = df %>%
    group_by(t, platform) %>%
    summarize(
      sv = sd(dfc),
      dfc = mean(dfc)
    )
  
  return(mn)
}

calc_distance_q = function(df){
  # compute distance quantiles at each timestep
  dq = df %>%
    group_by(t, platform) %>%
    summarize(
      q25 = quantile(dfc,0.25),
      q50 = quantile(dfc,0.5),
      q75 = quantile(dfc,0.75)
    )
  
  return(dq)
}

circle_coords = function(r = 1, x = 0, y = 0, npoints = 100){
  # calculate corodinates of a circle of radius r centered on x,y
  
  # create circle
  tt = seq(0,2*pi,length.out = npoints)
  xx = x + r * cos(tt)
  yy = y + r * sin(tt)
  
  # store coordinates
  coords = data.frame(x = xx, y = yy)
  return(coords)
}

combine_circle_coords = function(r_aco,r_vis){
  # process and combine circle coords for both platforms
  
  # acoustic
  aco = circle_coords(r_aco)
  aco$platform = 'acoustic'
  
  # visual
  vis = circle_coords(r_vis)
  vis$platform = 'visual'
  
  # combine
  out = rbind(aco,vis)
  
  return(out)
}

circle_quantiles = function(df,probs){
  # get coordinates for circles with radius of distance quantile computed from prob and df$dfc
  
  # calculate distance quantiles
  dq = quantile(df$dfc, probs = probs)
  
  # make circles
  tmp = vector('list',length(dq))
  for(ii in seq_along(dq)){
    tmp[[ii]] = circle_coords(dq[ii])
    tmp[[ii]]$id = probs[ii]
  }
  
  # collapse to data frame
  circ = bind_rows(tmp)
  
  # convert to ordered factor for plotting
  circ$id = factor(circ$id,levels = probs,ordered = TRUE)
  
  return(circ)
}

combine_circle_quantiles = function(df,probs){
  # run `circle_quantiles` for both platforms in df and combine
  
  # acoustic
  aco = circle_quantiles(subset(df,platform=='acoustic'),probs)
  aco$platform = 'acoustic'
  
  # visual
  vis = circle_quantiles(subset(df,platform=='visual'),probs)
  vis$platform = 'visual'
  
  # combine
  out = rbind(aco,vis)
  
  return(out)
}

plot_distance_m = function(df, bh, return_plot = FALSE){
  # plot mean distance vs time produced by `compare_rw_sims`
  
  message('\nMaking distance plot...')
  
  # bin distance
  mn = calc_distance_m(df)
  
  # plot mean and sd distances
  p1 = ggplot(mn)+
    geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), color = NA, alpha = 0.3)+
    geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
    labs(x = 'Time [hr]', y = 'Distance from center [km]', color = 'Mean:', fill = 'Standard deviation:')+
    theme_bw()+
    theme(legend.position = 'none')
  
  # define plot name
  pname = paste0('figures/', bh, '_distance_m.png')
  
  # save
  message('Writing plot...')
  ggsave(filename = pname, plot = p1, height = 6, width = 8, units = 'in', dpi = 250)
  
  # return plot
  if(return_plot){
    return(p1)  
  }
  
  message('Done! Plot saved as: ', pname)
}

plot_distance_q = function(df, bh, return_plot = FALSE){
  # plot distance quantile vs time produced by `compare_rw_sims`
  
  message('\nMaking quantile distance plot...')
  
  # compute quantiles
  dq = calc_distance_q(df)
  
  # plot distance quantiles
  p1 = ggplot(dq)+
    geom_ribbon(aes(x=t, ymin = q25, ymax = q75, fill=platform), color = NA, alpha = 0.3)+
    geom_path(aes(x=t, y=q50, color=platform), alpha = 1, size = 1)+
    labs(x = 'Time [hr]', y = 'Distance from center [km]', color = '50%:', fill = '25-75%')+
    theme_bw()+
    theme(legend.position = 'none')
  
  # define plot name
  pname = paste0('figures/', bh, '_distance_q.png')
  
  # save
  message('Writing plot...')
  ggsave(filename = pname, plot = p1, height = 6, width = 8, units = 'in', dpi = 250)
  
  # return plot
  if(return_plot){
    return(p1)  
  }
  
  message('Done! Plot saved as: ', pname)
}

make_movie = function(df, bh, movie_speed = 7){
  
  message('\nMaking movie plots...')
  
  # input parameters
  movie_name = paste0('figures/', bh,'.mp4')
  
  # calculate mean and standard deviations
  mn = calc_distance_m(df)
  
  # find data limits
  dmax = ceiling(max(df$dfc,na.rm = TRUE))
  tmax = ceiling(max(df$t,na.rm = TRUE))
  smax = ceiling(max(mn$dfc+mn$sv,na.rm = TRUE))
  
  # make plot directory
  plt_dir = paste0('figures/', bh, '/')
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
    a = df_i %>% filter(platform == 'acoustic') %>% pull(dfc)
    v = df_i %>% filter(platform == 'visual') %>% pull(dfc)
    
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
      geom_density(aes(x=dfc, color = platform, fill = platform), alpha = 0.2)+
      lims(x = c(0,dmax))+
      labs(x = 'Range [km]', y = 'Probability Density')+
      theme_bw()+
      annotate(geom = "text", x=Inf, y = Inf, vjust=1.5, hjust=1,
               label = paste0('Overlap: ', round(ovl*100), '%'))+
      theme(legend.position = 'none')
    
    # plot mean and sd
    p3 = ggplot(mn_i)+
      geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), 
                  color = NA, alpha = 0.3)+
      geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
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

make_movie_q = function(df, bh, movie_speed = 7){
  
  message('\nMaking movie plots...')
  
  # input parameters
  movie_name = paste0('figures/', bh,'.mp4')
  
  # calculate mean and standard deviations
  dq = calc_distance_q(df)
  
  # find data limits
  dmax = ceiling(max(df$dfc,na.rm = TRUE))
  tmax = ceiling(max(df$t,na.rm = TRUE))
  smax = ceiling(max(dq$q75,na.rm = TRUE))
  
  # make plot directory
  plt_dir = paste0('figures/', bh, '/')
  if(!dir.exists(plt_dir)){dir.create(plt_dir, recursive = TRUE)}
  
  # initialize plot variables
  tvec = seq(from = 0, to = tmax, by = 1)
  pb = txtProgressBar(min = 1, max = length(tvec), style = 3)
  
  # create movie plots
  for(it in seq_along(tvec)){
    
    # subset
    df_i = filter(df, t == tvec[it])
    dq_i = filter(dq, t <= tvec[it])
    
    # coord for circles
    # ave = combine_circle_quantiles(df_i,probs = .5)
    
    # breaks for colors
    brks = c(1,10,1e2,1e3,1e4,1e5)
    
    # plot maps
    p1 = ggplot()+
      # geom_point(data = df_i, aes(x=x, y=y), size = 0.2, alpha = 0.2)+
      geom_bin2d(data = df_i, aes(x=x, y=y, fill = stat(count)), binwidth = 1) +
      scale_fill_viridis_c(trans = "log", breaks = brks, labels = brks, limits=range(brks))+
      # geom_path(data = ave, aes(x=x,y=y), color = 'darkslategrey', size = 0.5)+
      coord_fixed()+
      xlim(c(-dmax,dmax))+
      ylim(c(-dmax,dmax))+
      facet_wrap(~platform)+
      labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Count')+
      theme_bw()+
      # theme(legend.position = "none")+
      guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))+
      ggtitle(paste0('Elapsed time = ', tvec[it], ' hrs'))
    
    # plot distances
    p2 = ggplot(dq_i)+
      geom_ribbon(aes(x=t, ymin = q25, ymax = q75, fill=platform), color = NA, alpha = 0.3)+
      geom_path(aes(x=t, y=q50, color=platform), alpha = 1, size = 1)+
      xlim(c(0,tmax))+
      ylim(c(0,smax))+
      labs(x = 'Time [hr]', y = 'Distance from center [km]', color = 'Platform', 
           fill = 'Platform')+
      theme_bw()
      # theme(legend.position = 'none')
    
    # make plot layout
    lay = rbind(c(1,1,1),
                c(1,1,1),
                c(2,2,2))
    
    # layer plots
    png(filename = paste0(plt_dir, 'hr_', sprintf("%02d",tvec[it]), '.png'), 
        height = 8, width = 9, units = 'in', res = 250)
    grid.arrange(p1,p2,layout_matrix = lay)
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

make_movie_m = function(df, bh, movie_speed = 7){
  # plot movie of data produced by `compare_rw_sims`
  
  message('\nMaking movie plots...')
  
  # input parameters
  movie_name = paste0('figures/', bh,'.mp4')
  
  # calculate mean and standard deviations
  mn = calc_distance_m(df)
  
  # find data limits
  dmax = ceiling(max(df$dfc,na.rm = TRUE))
  tmax = ceiling(max(df$t,na.rm = TRUE))
  smax = max(mn$dfc)+max(mn$sv)
  
  # make plot directory
  plt_dir = paste0('figures/', bh, '/')
  if(!dir.exists(plt_dir)){dir.create(plt_dir, recursive = TRUE)}
  
  # initialize plot variables
  tvec = seq(from = 0, to = tmax, by = 1)
  pb = txtProgressBar(min = 1, max = length(tvec), style = 3)
  
  # create movie plots
  for(it in seq_along(tvec)){
    
    # subset
    df_i = filter(df, t == tvec[it])
    mn_i = filter(mn, t <= tvec[it])
    cr_i = filter(mn_i, t == tvec[it])
    
    # coord for circles
    cir = combine_circle_coords(r_aco=cr_i$dfc[cr_i$platform=='acoustic'],
                                r_vis=cr_i$dfc[cr_i$platform=='visual'])
    
    # plot maps
    p1 = ggplot()+
      # geom_path(data = df_i, aes(x=x, y=y, group = id, color = platform), size = 0.1, alpha = 0.5)+
      # geom_point(data = df_i, aes(x=x, y=y, color=platform), size = 0.01, alpha = 0.05)+
      geom_point(data = df_i, aes(x=x, y=y), size = 0.2, alpha = 0.5)+
      geom_path(data = cir, aes(x=x,y=y,color = platform))+
      coord_fixed()+
      xlim(c(-dmax,dmax))+
      ylim(c(-dmax,dmax))+
      facet_wrap(~platform)+
      labs(x = 'Easting [km]', y = 'Northing [km]')+
      theme_bw()+
      theme(legend.position = "none")+
      ggtitle(paste0('Elapsed time = ', tvec[it], ' hrs'))
    
    # plot mean and sd distances
    p2 = ggplot(mn_i)+
      geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), color = NA, alpha = 0.3)+
      geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
      xlim(c(0,tmax))+
      ylim(c(0,smax))+
      labs(x = 'Time [hr]', y = 'Distance from center [km]', color = 'Mean:', fill = 'Standard deviation:')+
      theme_bw()+
      theme(legend.position = "bottom")
    
    # make plot layout
    lay = rbind(c(1,1,1),
                c(1,1,1),
                c(2,2,2))
    
    # layer plots
    png(filename = paste0(plt_dir, 'hr_', sprintf("%02d",tvec[it]), '.png'), 
        height = 8, width = 9, units = 'in', res = 250)
    grid.arrange(p1,p2,layout_matrix = lay)
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

proc_rw_sim = function(nrws = nrws, 
                       hrs = hrs,
                       nt=nt, 
                       bh = bh){
  # execute `compare_rw_sims`, then save and plot output
  
  message('\n##############################')
  message('## NARW MOVEMENT SIMULATION ##')
  message('##############################\n')
  
  # record start time
  tic = Sys.time()
  
  # simulate
  df = compare_rw_sims(nrws = nrws, hrs = hrs, nt=nt, bh = bh)
  
  # save data
  message('\nSaving data...')
  ofile = paste0('data/', bh, '.rda')
  save(nrws, dt, df, file = ofile)
  message('Done! Data saved as: ', ofile)
  
  # make distance plot
  # plot_distance_q(df, bh)
  
  # # make results movie
  # if(plot_movie){
  #   make_movie_q(df, bh)  
  # }
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  
  # remove df from memory
  rm(df)
  
  message('\n#############################')
  message('## SIMULATION COMPLETE :)  ##')
  message('#############################\n')
  message('Time elapsed: ', format(toc))
}

