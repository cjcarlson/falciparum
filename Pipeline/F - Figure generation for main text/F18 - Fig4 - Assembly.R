
library(patchwork)
top / middle / bottom + plot_layout(heights = c(2, 2, 1)) + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,1.5,0,0.5), units = "cm")))

map.rcp26.nolegend + map.rcp26.nolegend + map.rcp85.nolegend + legend + plot_layout(widths = c(4, 4, 4, 2)) -> top
map.rcp26.nolegend + map.rcp26.nolegend + map.rcp85.nolegend + legend + plot_layout(widths = c(4, 4, 4, 2)) -> middle
