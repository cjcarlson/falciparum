
library(patchwork)
top / bottom + plot_layout(heights = c(2, 1)) + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,1.5,0,0.5), units = "cm")))
