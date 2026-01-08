
top_row <- (g_with_hist + f + d + intervention_fig) +
  plot_layout(ncol = 4, widths = c(5, 5, 5, 2))

f2_alt <- top_row / s +  plot_annotation(tag_levels = 'A')

f2 <- (g_with_hist + f + d) / s + plot_annotation(tag_levels = 'A')

ggsave(
  filename = "Figure2_alt.pdf",
  plot = f2_alt,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  device = cairo_pdf,
  dpi = 1200
)

ggsave(
  filename = "Figure2_alt.jpg",
  plot = f2_alt,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)

# ggsave(
#   filename = "Figure2.pdf",
#   plot = f2,
#   path = here::here("Figures"),
#   width = 10.32,
#   height = 7.69,
#   units = "in",
#   device = cairo_pdf,
#   dpi = 1200
# )

# ggsave(
#   filename = "Figure2.jpg",
#   plot = f2,
#   path = here::here("Figures"),
#   width = 10.32,
#   height = 7.69,
#   units = "in"
# )

