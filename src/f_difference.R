## f_difference ##
# plot difference between visual and acoustic range estimates

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# cached output data file
cache_file = 'cache/difference_data.rda'

# number of right whales used in simulation
nrws = 1e5

# setup -------------------------------------------------------------------

library(cowplot)
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
  geom_path(data = dfs, aes(x=t,y=r,group=id), color = 'darkgrey', alpha = 0.3)+
  geom_path(data = mn, aes(x=t,y=r-er), color = 'black', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r+er), color = 'black', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r), color = 'black', size = 1)+
  labs(x = 'Time [h]', y = 'Visual - Acoustic range [km]')+
  geom_hline(yintercept = 0, col = 'lightgrey', linetype = 1)+
  facet_grid(bh~run)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# plot proportional differences -------------------------------------------

# plot
p2 = ggplot()+
  geom_path(data = prp, aes(x=t,y=p,group=bh,linetype=bh))+
  scale_linetype_manual(values = c('traveling' = 1, 'feeding' = 2, 'socializing' = 3))+
  labs(x = 'Time [h]', 
       y = 'Probability of higher uncertainty\nfrom visual detection', 
       linetype = 'Behavior')+
  facet_wrap(~run, nrow = 1, ncol = 3)+
  scale_x_continuous(breaks = c(0,24,48,72,96))+
  theme_bw()

# combine -----------------------------------------------------------------

# combine and align
p3 = plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1.5,0.9), labels = c('(a)','(b)'))

# save
ggsave(filename = 'figures/f_difference.pdf', plot = p3, width = 7, height = 8, dpi = 800)  

  