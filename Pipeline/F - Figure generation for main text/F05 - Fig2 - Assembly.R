(g + f + d) / s + plot_annotation(tag_levels = 'A')

# (combined_plot + f + d) / s + plot_annotation(tag_levels = 'A')

# combined_plot <- plot_grid(
#   g +                                       # dose–response “spaghetti”
#     theme(
#       axis.text.x  = element_blank(),
#       axis.line.x  = element_blank(),
#       axis.ticks.x = element_blank(),
#       axis.title.x = element_blank()
#     ),
#   h + labs(x = expression(paste("Mean temperature (", degree, "C)"))),
#   ncol       = 1,            # stack vertically
#   align      = "v",          # share x‑axis
#   rel_heights = c(10, 1)     # tall curve, short histogram
# )

upper_row <- plot_grid(
  combined_plot,
  f,
  d,
  nrow = 1,
  # align       = "h",
  # axis        = "tb",
  rel_widths = c(1, 1, 1),
  rel_heights = c(.9, 1, 1),
  labels = c("A", "B", "C"),
  label_size = 12,
  label_fontface = "bold"
)

final_fig <- plot_grid(
  upper_row,
  s,
  ncol = 1,
  align = "h",
  rel_heights = c(1, 1), # upper row twice the height of bottom row
  labels = c("", "D"), # only “D” on the bottom panel
  label_size = 12,
  label_fontface = "bold"
)

# Preview
final_fig


# ggsave(
#   filename = "Figure2_revision.pdf",
#   plot = last_plot(),
#   path = here::here("Figures"),
#   width = 10.32,
#   height = 7.69,
#   units = "in",
#   device = cairo_pdf,
#   dpi = 1200
# )

ggsave(
  filename = "Figure2_revision.jpg",
  plot = last_plot(),
  path = here::here("Figures"),
  width = 10.32,
  height = 7.69,
  units = "in"
)
