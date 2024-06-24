
library(patchwork)
# top / bottom + plot_layout(heights = c(2, 1)) + 
#   plot_annotation(theme = theme(plot.margin = unit(c(0,1.5,0,0.5), units = "cm")))


part1 <- paste(replicate(151, "\nAAAAAAAAABBCC"), collapse = "")
part2 <- "\nAAAAAAAAA####\n"
part3 <- paste(replicate(80, "DDDDDDDDDDDDD\n"), collapse = "")
layout <- paste(part1, part2, part3, sep = "")

map.diff + g3 + g1 + bottom +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))

ggsave(
  filename = "Figure3_06-2024.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
  dpi = 600
)


# layout <- "
# AAAAAAA####
# AAAAAAABBCC
# AAAAAAABBCC
# AAAAAAABBCC
# AAAAAAABBCC
# AAAAAAA####
# DDDDDDDDDDD
# DDDDDDDDDDD
# "
# 
# map.diff + g3 + g1 + bottom + 
#   plot_layout(design = layout,
#               heights = c(0.2,1,1,1,1,0.2,1,1)) + 
#   plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 23))
