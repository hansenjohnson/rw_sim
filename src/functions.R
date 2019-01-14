## functions ##
# source code for rw_sim analysis

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(gridExtra)

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
  sub = TRUE,     # subsample data to new rate, nt
  nt = 60         # new time resolution after subsampling [sec]
){
  # simulate right whale movement with vectorized correlated random walk
  
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
  
  # calculate distance from center
  df$dfc = sqrt(df$x^2 + df$y^2)
  
  return(df)
}

rw_sims = function(nrws = 100, 
                   hrs = 48, 
                   bh = 'feeding', 
                   radius = 5e3, 
                   nt = 300, 
                   convert2hr = TRUE, 
                   convert2km = TRUE){
  # iteratively call `rw_sim` to simulate movements of many whales
  
  # message
  message('\n## NARW MOVEMENT MODEL RUN ##\n')
  message('Input parameters:')
  message('   number of whales: ', nrws)
  message('   number of hours: ', hrs)
  message('   time resolution [sec]: ', nt)
  message('   number of timesteps: ', hrs*60*60/nt)
  message('   starting radius [m]: ', radius)
  message('   movement type: ', bh)
  message('Simulating whale movements...')
  
  # initialize data values
  DF = vector('list', length(nrws))
  pb = txtProgressBar(min = 1, max = nrws, style = 3)
  tic = Sys.time()
  # simulate nrws
  for(jj in 1:nrws){
    
    # randomly select starting point within circle
    a=2*pi*runif(1,0,1)
    r=sqrt(runif(1,0,1))
    x0=(radius*r)*cos(a)
    y0=(radius*r)*sin(a)
    
    # simulate single whale
    tmp = rw_sim(x0 = x0, y0 = y0, hrs = hrs, bh = bh, nt=nt)
    
    # add whale ID
    tmp$id = jj
    
    # add to list
    DF[[jj]] = tmp
    
    # update progress bar
    setTxtProgressBar(pb, jj)
  }
  
  message('\nFormatting output data...')
  
  # flatten list to combine all whales
  df = bind_rows(DF)
  
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
                           a_radius = 15e3, 
                           v_radius = 0.5e3,
                           bh = 'feeding', 
                           nt = 300){
  # call `rw_sims` for two starting radii
  
  # acoustic
  df1 = rw_sims(nrws = nrws, hrs = hrs, radius = a_radius, bh = bh, nt=nt)
  df1$platform = 'Acoustic'
  
  # visual
  df2 = rw_sims(nrws = nrws, hrs = hrs, radius = v_radius, bh = bh, nt=nt)
  df2$platform = 'Visual'
  
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

bin_distance = function(df){
  
  mn = df %>%
    group_by(t, platform) %>%
    summarize(
      sv = sd(dfc),
      dfc = mean(dfc)
    )
  
  return(mn)
}

distance_plot = function(df, bh, return_plot = FALSE){
  # plot distance vs time produced by `compare_rw_sims`
  
  message('\nMaking distance plot...')
  
  # bin distance
  mn = bin_distance(df)
  
  # plot mean and sd distances
  p1 = ggplot(mn)+
    geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), color = NA, alpha = 0.3)+
    geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
    labs(x = 'Time [hr]', y = 'Distance from center [km]', color = 'Mean:', fill = 'Standard deviation:')+
    theme_bw()+
    theme(legend.position = 'bottom')
  
  # define plot name
  pname = paste0('figures/', bh, '_distance.png')
  
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
  # plot movie of data produced by `compare_rw_sims`
  
  message('\nMaking movie plots...')
  
  # input parameters
  movie_name = paste0('figures/', bh,'.mp4')
  
  # calculate mean and standard deviations
  mn = bin_distance(df)
  
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
    
    # plot maps
    p1 = ggplot(df_i)+
      # geom_path(data = df_i, aes(x=x, y=y, group = id, color = platform), size = 0.1, alpha = 0.5)+
      # geom_point(data = df_i, aes(x=x, y=y, color=platform), size = 0.01, alpha = 0.05)+
      geom_point(aes(x=x, y=y), size = 0.2, alpha = 0.5)+
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
                       a_radius = a_radius, 
                       v_radius = v_radius, 
                       nt=nt, 
                       bh = bh,
                       plot_movie = TRUE){
  # execute `compare_rw_sims`, then save and plot output
  
  message('\n##############################')
  message('## NARW MOVEMENT SIMULATION ##')
  message('##############################\n')
  
  # record start time
  tic = Sys.time()
  
  # simulate
  df = compare_rw_sims(nrws = nrws, a_radius = a_radius, v_radius = v_radius, nt=nt, bh = bh)
  
  # save data
  message('\nSaving data...')
  ofile = paste0('data/', bh, '.rda')
  save(nrws, a_radius, v_radius, dt, df, file = ofile)
  message('Done! Data saved as: ', ofile)
  
  # # make summary plot
  # summary_plot(df, bh)
  
  # make distance plot
  distance_plot(df, bh)
  
  # make results movie
  if(plot_movie){
    make_movie(df, bh)  
  }
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  
  message('\n#############################')
  message('## SIMULATION COMPLETE :)  ##')
  message('#############################\n')
  message('Time elapsed: ', format(toc))
}
