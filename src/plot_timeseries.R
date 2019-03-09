## plot_timeseries ##

# setup -------------------------------------------------------------------

source('src/functions.R')

# bhs = c('Linear','Traveling','Feeding','Socializing','Random')
bhs = c('Traveling','Feeding','Socializing')

# compare behaviours ------------------------------------------------------

# loop through behaviours and process
DQ = vector('list', length(bhs))
for(ii in seq_along(bhs)){
  
  ibh = bhs[ii]
  load(paste0('data/',tolower(ibh),'.rda'))
  
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
  geom_ribbon(aes(x=t, ymin = dfc-sv, ymax = dfc+sv, fill=platform), color = NA, alpha = 0.3)+
  geom_path(aes(x=t, y=dfc, color=platform), alpha = 1, size = 1)+
  labs(x = 'Time [hr]', y = 'Range [km]', fill = NULL, color = NULL)+
  theme_bw()+
  facet_wrap(~bh, ncol = 1, scales = "free_y")+
  theme(legend.position = "bottom", legend.direction = 'horizontal')

# save
ggsave(plot = p1,filename = 'figures/timeseries.png', width = 5, height = 7, dpi = 300)