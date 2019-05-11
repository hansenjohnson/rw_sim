## plot_time_space ##

library(tidyverse)
library(grid)

# define plot function
plot_time_space = function(bhv){
  # read in data
  load(paste0('data/', tolower(bhv), '.rda'))
  
  # subset
  df_i = filter(df, t %in% c(0,6,12,24,48,96))
  
  # plot breaks
  brks = c(1,10,1e2,1e3,1e4,1e5)
  lim = max(df_i$r)
  
  plt = ggplot(df_i, aes(x=x, y=y))+
    geom_bin2d(aes(fill = stat(count)), binwidth = 1) +
    scale_fill_viridis_c(trans = "log", breaks = brks, labels = brks, limits=range(brks))+
    labs(x = 'Easting [km]', y = 'Northing [km]', fill = 'Count', title = bhv)+
    facet_grid(platform~t)+
    xlim(c(-lim,lim))+
    ylim(c(-lim,lim))+
    coord_equal()+
    theme_bw()+
    theme(plot.margin=unit(c(0,0.5,0.5,0), "cm"))+
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15))
  
  ggsave(filename = paste0('figures/time_space_',tolower(bhv), '.png'),plot = plt, 
         height = 6, width = 12, dpi = 300)
  
  return(plt)
}

# plot
ln = plot_time_space('Linear')
tr = plot_time_space('Traveling')
fd = plot_time_space('Feeding')
sc = plot_time_space('Socializing')
rn = plot_time_space('Random')

# plot combined
png(filename = 'figures/time_space_all.png', width = 16, height = 20, units = 'in', res = 300)
grid.newpage()
grid.draw(rbind(ggplotGrob(ln), ggplotGrob(tr), ggplotGrob(fd),ggplotGrob(sc),ggplotGrob(rn), size = "last"))
dev.off()
