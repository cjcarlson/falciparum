

(g + f + d) / s + plot_annotation(tag_levels = 'A')


ggsave(
  filename = "Figure2_new.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  dpi = 1200
)
