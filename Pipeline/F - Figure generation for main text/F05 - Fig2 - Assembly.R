
(g + f + d) / s + plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2_test.pdf",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2_test.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)




top_row <- (g_with_hist + f + d + intervention_fig) +
  plot_layout(ncol = 4, widths = c(5, 5, 5, 2))

f2_alt <- top_row / s +  plot_annotation(tag_levels = 'A')

f2 <- (g_with_hist + f + d) / s + plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2_test.pdf",
  plot = f2_alt,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2_test.jpg",
  plot = f2_alt,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)
