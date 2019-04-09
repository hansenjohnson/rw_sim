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
  y = detection_function(x=xx, x0=5, L=1.13, k=-0.4)
)

# medium range
r2 = data.frame(
  x = xx,
  y = detection_function(x=xx, x0=10, L=1.045, k=-0.3)
)

# long range
r3 = data.frame(
  x = xx,
  y = detection_function(x=xx, x0=15, L=1.02, k=-0.25)
)

# plot detection function
p1 = ggplot()+
  geom_path(data = r1, aes(x=x,y=y), linetype=3)+
  geom_path(data = r2, aes(x=x,y=y), linetype=2)+
  geom_path(data = r3, aes(x=x,y=y), linetype=1)+
  labs(x = 'Range [km]', y = 'Probability of detection')+
  theme_bw()

# save
ggsave(p1, filename = 'figures/f_detection_function.png', width = 6, height = 4, dpi = 300)
