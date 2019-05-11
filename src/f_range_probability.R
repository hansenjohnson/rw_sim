## f_range_probability ##
# figure of location probability with a range radius given behaviour and time

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

# extract probabilities
DQ = vector('list', length(rns)*length(bhs))
cnt = 1
for(ii in seq_along(rns)){
  irun = rns[ii]
  for(jj in seq_along(bhs)){
    ibhs = bhs[jj]
    
    # load data
    ifile = paste0('runs/',irun,'/data/',ibhs,'.rda')
    message('Processing file ', cnt, ' of ', length(DQ), ':\n', ifile)
    load(ifile)
    
    # compute p
    DQ[[cnt]] = df %>%
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
        run = irun,
        bh = ibhs
      )
    cnt = cnt+1
  }
}

# combine
dq = bind_rows(DQ)

# rename factor levels for plotting
levels(dq$dist) = c('5km', '10km', '15km', '25km')
dq$bh = factor(dq$bh, levels = bhs, ordered = TRUE)

# add column for plotting
dq$grp = paste0(dq$platform,'-',dq$run)

# plot
p1 = ggplot(dq)+
  geom_path(aes(x=t,y=p,group=grp,color=platform, linetype=run))+
  scale_linetype_manual(values = c('long' = 1, 'medium' = 2, 'short' = 3))+
  ylim(c(0,1))+
  facet_grid(bh~dist)+
  labs(x = 'Time [hr]', y = 'Probability', color = 'Platform:', linetype = 'Detection range:')+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = 'horizontal')

# save
ggsave(plot = p1, filename = 'figures/f_range_probability.png', 
       width = 8, height = 6, dpi = 300)