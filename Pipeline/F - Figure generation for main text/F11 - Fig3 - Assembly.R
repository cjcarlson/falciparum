
library(patchwork)
top / bottom + plot_layout(heights = c(2, 1)) + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,1.5,0,0.5), units = "cm")))



layout <- "
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
DDDDDDDDDDD
DDDDDDDDDDD
"

map.diff + g3 + g1 + bottom + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))




layout <- "
AAAAAAA####
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
AAAAAAABBCC
AAAAAAA####
DDDDDDDDDDD
DDDDDDDDDDD
"

map.diff + g3 + g1 + bottom + 
  plot_layout(design = layout,
              heights = c(0.2,1,1,1,1,0.2,1,1)) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))
