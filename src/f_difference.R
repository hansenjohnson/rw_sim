## f_difference ##
# plot difference between visual and acoustic range estimates

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# cached output data file
cache_file = 'cache/difference_data.rda'

# number of right whales
nrws = 1e5

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

if(!file.exists(cache_file)){
  
  # extract data
  DF = vector('list', length(rns)*length(bhs))
  MN = vector('list', length(rns)*length(bhs))
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
      
      # calculate range difference by id
      DF[[cnt]] = df[c('id','t','platform','r','bh')] %>%
        spread(key = platform,value = r) %>%
        mutate(r = visual - acoustic,
               run = irun)
      
      # calculate mean range difference
      MN[[cnt]] = DF[[cnt]] %>%
        group_by(t,bh,run) %>%
        summarize(
          er = sd(r, na.rm = T),
          r = mean(r, na.rm = T)
        )
      
      rm(df)
      cnt = cnt+1
      message('\nDone!')
    }
  }
  
  # combine
  df = bind_rows(DF)
  mn = bind_rows(MN)
  
  # define factors for plot order
  df$bh = factor(df$bh)
  df$bh = factor(df$bh, levels = c('traveling', 'feeding', 'socializing'), ordered = T)
  
  # define factors for plot order
  mn$bh = factor(mn$bh)
  mn$bh = factor(mn$bh, levels = c('traveling', 'feeding', 'socializing'), ordered = T)
  
  # score as above or below 0
  df$s = NA
  df$s[df$r > 0] = '>0'
  df$s[df$r < 0] = '<0'
  
  # calculate proportion
  prp = df %>%
    group_by(t,bh,run) %>%
    count(s) %>%
    filter(s == '<0' & bh %in% bhs) %>%
    mutate(p = 1-n/nrws)
  
  # save data
  save(df, mn, prp, file = cache_file)
  
} else {
  message('Using data saved in: ', cache_file)
  message('Delete to re-process...')
  load(cache_file)
}

# plot range differences --------------------------------------------------

# only plot a subset (or computer will crash)
dfs = df %>%
  mutate(id = as.numeric(id)) %>%
  filter(id <= 1e3)

# plot
p1 = ggplot()+
  geom_path(data = dfs, aes(x=t,y=r,group=id), alpha = 0.1)+
  geom_path(data = mn, aes(x=t,y=r-er), color = 'red', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r+er), color = 'red', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r), color = 'red', size = 1)+
  labs(x = 'Time [h]', y = 'Visual - Acoustic range [km]')+
  geom_hline(yintercept = 0, col = 'grey', linetype = 1)+
  facet_grid(bh~run)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()

ggsave(filename = 'figures/f_difference.png', plot = p1, width = 10, height = 8, dpi = 300)

p1_mn = ggplot()+
  # geom_path(data = dfs, aes(x=t,y=r,group=id), alpha = 0.1)+
  geom_path(data = mn, aes(x=t,y=r-er), color = 'black', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r+er), color = 'black', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r), color = 'black', size = 1)+
  labs(x = 'Time [h]', y = 'Visual - Acoustic range [km]')+
  geom_hline(yintercept = 0, col = 'grey', linetype = 1)+
  facet_grid(bh~run)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()

ggsave(filename = 'figures/f_difference-lrg.png', plot = p1_mn, width = 6, height = 5, dpi = 300)

# plot proportional differences -------------------------------------------

# plot
p2 = ggplot()+
  geom_path(data = prp, aes(x=t,y=p,group=bh,color=bh))+
  scale_color_manual(values = c('black', 'blue', 'red'))+
  labs(x = 'Time [h]', y = 'Probability of higher uncertainty from visual detection', color = 'Behaviour')+
  facet_wrap(~run, nrow = 1, ncol = 3)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()

# save
ggsave(filename = 'figures/f_difference-proportion.png', plot = p2, width = 10, height = 4, dpi = 300)

# save large version
p3 = p2+labs(y='P of higher visual uncertainty', color = 'Behaviour:')+theme(legend.position = "bottom")
ggsave(filename = 'figures/f_difference-proportion-lrg.png', plot = p3, width = 6, height = 3, dpi = 300)
