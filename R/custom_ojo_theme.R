custom_ojo_theme <- function() {
  ggplot2::theme_bw(
    base_size = 16,
    base_family = "roboto_condensed"
  ) %+replace%
  ggplot2::theme(
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_blank(),
  legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
  legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
  legend.position = "top"
  ) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
}
