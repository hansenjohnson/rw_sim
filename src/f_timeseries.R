## f_timeseries ##
# figure comparing range timeseries' among runs

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# cached output data file
cache_file = 'cache/timeseries_data.rda'

# setup -------------------------------------------------------------------

library(tidyverse)

# process -----------------------------------------------------------------

if(!file.exists(cache_file)){
  
  # extract data
  DF = vector('list', length(rns)*length(bhs))
  cnt = 1
  for(ii in seq_along(rns)){
    irun = rns[ii]
    for(jj in seq_along(bhs)){
      ibhs = bhs[jj]
      ifile = paste0('runs/',irun,'/',ibhs,'.rda')
      
      # read in data
      message('Processing file ', cnt, ' of ', length(DF), ':\n', ifile)
      
      # read in data
      message('   Loading...')
      load(ifile)
      
      # calculate median and quantiles
      DF[[cnt]] = df %>%
        group_by(t, platform) %>%
        summarize(
          lwr = quantile(r,0.25),
          med = quantile(r,0.5),
          upr = quantile(r,0.75),
          run = irun,
          bh = ibhs
        )
      
      rm(df)
      cnt = cnt+1
      message('\nDone!')
    }
  }
  
  # combine
  df = bind_rows(DF)
  
  # save data
  save(df, file = cache_file)
  
} else {
  message('Using data saved in: ', cache_file)
  message('Delete to re-process...')
  load(cache_file)
}

max_vis = df %>%
  group_by(run,bh) %>%
  filter(t == 96 & run == 'medium' & platform == 'visual') %>%
  mutate(iqr = upr-lwr)

print(max_vis)

min_aco = df %>%
  group_by(run,bh) %>%
  filter(t == 0 & platform == 'acoustic')

print(min_aco)

# timeseries with overlap and residuals -----------------------------------

# define factors for plot order
df$bh = factor(df$bh)
df$bh = factor(df$bh, levels = c('traveling', 'feeding', 'socializing'), ordered = T)

# plot
p1 = ggplot(df)+
  geom_ribbon(aes(x=t, ymin = lwr, ymax = upr, fill=platform),
              color = NA, alpha = 0.3)+
  geom_path(aes(x=t, y=med, color=platform), alpha = 1, size = 1)+
  facet_grid(bh~run, scales = "free")+
  labs(x = 'Time [hr]', y = 'Range [km]', fill = NULL, color = NULL)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = 'horizontal')

ggsave(p1, filename = 'figures/f_timeseries.png', width = 10, height = 8, dpi = 300)