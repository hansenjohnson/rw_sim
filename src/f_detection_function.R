## f_detection_function ##
# figure comparing acoustic detection functions

library(tidyverse)
source('src/functions.R')

# maximum range to plot
rmax = 40

# range vector
xx = seq(from = 0, to = rmax, by = 0.1)

# short range
r1 = data.frame(
  x = xx,
  y = detection_function(x=xx, x0=5, L=1.13, k=-0.4),
  g = 'short'
)

# medium range
r2 = data.frame(
  x = xx,
  y = detection_function(x=xx, x0=10, L=1.045, k=-0.3),
  g = 'medium'
)

# long range
r3 = data.frame(
  x = xx,
  y = detection_function(x=xx, x0=15, L=1.02, k=-0.25),
  g = 'long'
)

# combine
rf = rbind(r1,r2,r3)

# plot detection function
p1 = ggplot(data = rf)+
  geom_path(aes(x=x,y=y,group=g,linetype=g))+
  scale_linetype_manual(values = c(3,2,1))+
  labs(x = 'Range [km]', y = 'Probability of detection', linetype = NULL)+
  theme_bw()+
  theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.background = element_blank())

# save
ggsave(p1, filename = 'figures/f_detection_function.jpg', width = 6, height = 4, dpi = 300)
