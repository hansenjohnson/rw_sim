## combine_distance_plots ##

# setup -------------------------------------------------------------------

source('src/functions.R')

bhs = c('Linear','Traveling','Feeding','Socializing','Random')

# compare behaviours ------------------------------------------------------

# loop through behaviours and process
DQ = vector('list', length(bhs))
for(ii in seq_along(bhs)){
  
  ibh = bhs[ii]
  load(paste0('data/',tolower(ibh),'.rda'))
  
  idq = calc_distance_q(df)
  idq$bh = ibh
  
  DQ[[ii]] = idq
}

# combine
dq = bind_rows(DQ)

# arrange factors
dq$bh = factor(dq$bh, levels = bhs, ordered = TRUE)

# plot
p1 = ggplot(dq)+
  geom_ribbon(aes(x=t, ymin = q25, ymax = q75, fill=platform), color = NA, alpha = 0.3)+
  geom_path(aes(x=t, y=q50, color=platform), alpha = 1, size = 1)+
  labs(x = 'Time [hr]', y = 'Distance from center [km]', fill = NULL, color = NULL)+
  theme_bw()+
  facet_wrap(~bh, ncol = 2, scales = "free_y")+
  theme(legend.position = "bottom", legend.direction = 'horizontal')

# save
ggsave(plot = p1,filename = 'figures/timeseries.png', width = 8, height = 10, dpi = 300)

# The probability that a whale in a given behavioural state (right) is within a given range
# of the incident observation location (top) over 96 hours following the observation
