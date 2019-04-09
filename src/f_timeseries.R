## f_timeseries ##
# figure comparing range timeseries' among runs

# input -------------------------------------------------------------------

# list run names
rns = c('long','medium','short')

# list behaviours
bhs = c('traveling','feeding','socializing')

# setup -------------------------------------------------------------------

source('src/functions.R')

# process -----------------------------------------------------------------

# extract data

DF = vector('list', length(rns)*length(bhs))
OVL = vector('list', length(DF))
KS = vector('list', length(DF))
AR = vector('list', length(DF))
cnt = 1
for(ii in seq_along(rns)){
  irun = rns[ii]
  for(jj in seq_along(bhs)){
    ibhs = bhs[jj]
    ifile = paste0('runs/',irun,'/data/',ibhs,'.rda')
    
    # read in data
    message('Processing file ', cnt, ' of ', length(DF), ':\n', ifile)
    
    # read in data
    message('   Loading...')
    load(ifile)
    
    # calculate quantile distances
    message('   Calculating distance quantiles...')
    DF[[cnt]] = calc_distance_q(df, lower = 0.025, upper = 0.975) %>%
      mutate(run = irun,
             bh = ibhs)
    
    # calculate overlap between pdfs
    message('   Calculating overlap between PDFs...')
    OVL[[cnt]] = calc_overlap(df) %>%
      mutate(run = irun,
             bh = ibhs)
    
    # calculate ks test between pdfs
    message('   Calculating KS test...')
    KS[[cnt]] = calc_ks(df) %>%
      mutate(run = irun,
             bh = ibhs)
    
    # calculate acoustic residuals
    message('   Calculating acoustic residuals...')
    AR[[cnt]] = calc_acoustic_residuals(df) %>%
      mutate(run = irun,
             bh = ibhs)
    
    cnt = cnt+1
    message('\nDone!')
  }
}

# combine
df = bind_rows(DF)
ovl = bind_rows(OVL)
ks = bind_rows(KS)
ar = bind_rows(AR)

# calculate overlap by group
ov = ovl %>%
  group_by(run,bh) %>%
  summarise(
    tov = hrs[which.max(round(ovl,2)>=0.90)]
  )
# replace zeros with NAs
ov$tov[ov$tov==0] = NA

print(ov)

# calculate resid by group
gar = ar %>%
  group_by(run,bh) %>%
  summarise(
    tar = hrs[which.max(ar<=0.1)]
  )
# replace zeros with NAs
gar$tar[gar$tar==0] = NA

print(gar)

max_vis = df %>%
  group_by(run,bh) %>%
  filter(t == 96 & run == 'medium' & platform == 'visual')

print(max_vis)

min_aco = df %>%
  group_by(run,bh) %>%
  filter(t == 0 & platform == 'acoustic')

print(min_aco)

# save data
save(df,ovl,ks,ar, file = 'figures/f_timeseries_data.rda')

# plot --------------------------------------------------------------------

# plot overlap
p1 = ggplot(ovl)+
  geom_path(aes(x=hrs, y=ovl), alpha = 1, size = 1)+
  facet_grid(bh~run)+
  labs(x = 'Time [hr]', y = 'Overlap [%]', fill = NULL, color = NULL)+
  theme_bw()

ggsave(p1, filename = 'figures/s_overlap.png', width = 10, height = 8, dpi = 300)

# plot ks
p2 = ggplot(ks)+
  geom_path(aes(x=hrs, y=ks), alpha = 1, size = 1)+
  facet_grid(bh~run)+
  labs(x = 'Time [hr]', y = 'P-value', fill = NULL, color = NULL)+
  theme_bw()

ggsave(p2, filename = 'figures/s_ks_test.png', width = 10, height = 8, dpi = 300)

# plot residuals
p3 = ggplot(ar)+
  geom_path(aes(x=hrs, y=ar), alpha = 1, size = 1)+
  facet_grid(bh~run)+
  labs(x = 'Time [hr]', y = 'P', fill = NULL, color = NULL)+
  theme_bw()

ggsave(p3, filename = 'figures/s_acoustic_residuals.png', width = 10, height = 8, dpi = 300)

# timeseries with overlap and residuals -----------------------------------

# # define factors for plot order
# df$bh = factor(df$bh)
# df$bh = factor(df$bh, levels = c('traveling', 'feeding', 'socializing'), 
#                labels = c('Traveling', 'Feeding', 'Socializing'), ordered = T)
# df$run = factor(df$run)
# df$run = factor(df$run, labels = c('Long', 'Medium', 'Short'), ordered = T)

p4 = ggplot(df)+
  geom_ribbon(aes(x=t, ymin = lwr, ymax = upr, fill=platform),
              color = NA, alpha = 0.3)+
  geom_path(aes(x=t, y=med, color=platform), alpha = 1, size = 1)+
  geom_vline(data = gar, aes(xintercept = tar), linetype = 2, color = 'darkgrey') +
  geom_vline(data = ov, aes(xintercept = tov), linetype = 3, color = 'brown') +
  facet_grid(bh~run, scales = "free")+
  labs(x = 'Time [hr]', y = 'Range [km]', fill = NULL, color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = 'horizontal', 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(p4, filename = 'figures/f_timeseries.png', width = 10, height = 8, dpi = 300)
