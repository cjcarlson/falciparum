

layout <- "
ABCG
DEFG
HHHH
"

map.rcp26.2050 + map.rcp45.2050 + map.rcp85.2050 + 
  map.rcp26.2100 + map.rcp45.2100 + map.rcp85.2100 + 
  legend + bottom + plot_layout(design = layout, widths = c(4, 4, 4, 2), heights = c(2, 2, 1.5)) + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,0,0,0.1), units = "cm")))

