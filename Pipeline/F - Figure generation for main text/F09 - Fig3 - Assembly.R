
library(patchwork)

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

map.diff + g3 + g1 + bottom +
  plot_layout(design = fig_3_4_layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))

ggsave(
  filename = "Figure3.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
  dpi = 1200
)
