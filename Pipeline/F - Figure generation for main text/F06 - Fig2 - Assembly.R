
# Old draft that included maps now in Figure 1 
#(map.n + map.p) / (g + f + d) / s + 
#  plot_layout(heights = c(2.25, 1, 1)) + plot_annotation(tag_levels = 'A')

# no map

(g + f + d) / s + plot_annotation(tag_levels = 'A')


ggsave(
  filename = "Figure2_06-2024.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in",
  dpi = 600
)
