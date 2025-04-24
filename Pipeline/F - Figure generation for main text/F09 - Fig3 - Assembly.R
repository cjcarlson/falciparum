map.diff +
  g3 +
  g1 +
  bottom +
  plot_layout(design = fig_3_4_layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))

ggsave(
  filename = "Figure3.pdf",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure3.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 11.63,
  height = 10.07,
  units = "in",
)
