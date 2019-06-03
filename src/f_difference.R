## f_difference ##
# plot difference between visual and acoustic range estimates

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# cached output data file
cache_file = 'cache/difference_data.rda'

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
  
  # save data
  save(df, mn, file = cache_file)
  
} else {
  message('Using data saved in: ', cache_file)
  message('Delete to re-process...')
  load(cache_file)
}

# plot range differences --------------------------------------------------

# plot all and mean differences
p1 = ggplot()+
  geom_path(data = df, aes(x=t,y=r,group=id), alpha = 0.01)+
  geom_path(data = mn, aes(x=t,y=r-er), color = 'blue', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r+er), color = 'blue', linetype = 2)+
  geom_path(data = mn, aes(x=t,y=r), color = 'red', size = 1.5)+
  labs(x = 'Time [h]', y = 'Visual - Acoustic range [km]')+
  geom_hline(yintercept = 0, col = 'grey', linetype = 1)+
  facet_wrap(bh~run, nrow = 2, ncol = 3)+
  theme_bw()

ggsave(filename = paste0(odir,'/f_difference.png'), plot = p1, width = 10, height = 8, dpi = 300)

# plot proportional differences -------------------------------------------

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

# plot
p2 = ggplot()+
  geom_path(data = prp, aes(x=t,y=p,group=bh,color=bh))+
  scale_color_manual(values = c('black', 'blue', 'red'))+
  labs(x = 'Time [h]', y = 'Probability of higher uncertainty from acoustic detection', color = 'Behaviour')+
  facet_wrap(~run, nrow = 1, ncol = 3)+
  theme_bw()

ggsave(filename = paste0(odir,'/f_difference-proportion.png'), plot = p2, width = 7, height = 5, dpi = 300)

