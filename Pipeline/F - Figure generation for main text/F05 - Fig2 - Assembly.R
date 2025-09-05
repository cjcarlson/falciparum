f2 <- (g_with_hist + f + d) / s + plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2.pdf",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2.jpg",
  plot = f2,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)
