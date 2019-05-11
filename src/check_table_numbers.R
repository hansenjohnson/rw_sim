## check_table_numbers ##

library(tidyverse)

load('figures/f_timeseries_data.rda')

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
