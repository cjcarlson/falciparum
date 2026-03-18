
top_row <- (g_with_hist + f + d + intervention_fig) +
  plot_layout(ncol = 4, widths = c(5, 5, 5, 2))

f2 <- top_row / s +  plot_annotation(tag_levels = 'A')

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

