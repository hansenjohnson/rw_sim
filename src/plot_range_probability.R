## range_probability_plot ##

# setup -------------------------------------------------------------------

source('src/functions.R')

# compare behaviours ------------------------------------------------------

bhs = c('Traveling','Feeding','Socializing')

DQ = vector('list', length(bhs))
for(ii in seq_along(bhs)){
  
  ibh = bhs[ii]
  load(paste0('data/',tolower(ibh),'.rda'))
  
  # compute p
  DQ[[ii]] = df %>%
    group_by(t, platform) %>%
    summarize(
      p5 = length(dfc[dfc<=5])/length(dfc),
      p10 = length(dfc[dfc<=10])/length(dfc),
      p15 = length(dfc[dfc<=15])/length(dfc),
      p25 = length(dfc[dfc<=25])/length(dfc)
    ) %>%
    gather(key = dist, value = p, p5:p25) %>%
    mutate(
      dist = factor(dist, levels = c('p5','p10','p15','p25'),ordered = TRUE),
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

ggsave(plot = p1,filename = 'figures/range_probability.png', width = 10, height = 8.5, dpi = 300)

# The probability that a whale in a given behavioural state (right) is within a given range
# of the incident observation location (top) over 96 hours following the observation
