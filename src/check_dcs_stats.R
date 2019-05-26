## check_dcs_stats ##
# quick check of numbers of days and right whale detections

library(tidyverse)

# effort
trk = readRDS('~/Projects/WhaleMap/data/processed/tracks.rds') %>%
  filter(platform %in% c('slocum', 'buoy')) %>%
  group_by(id) %>%
  summarize(days = length(unique(date)))

# total days at sea
sum(trk$days)

# detections
obs = readRDS('~/Projects/WhaleMap/data/processed/observations.rds') %>%
  filter(platform %in% c('slocum', 'buoy') & species == 'right' & score == 'definite acoustic')

# total days at sea
nrow(obs)
