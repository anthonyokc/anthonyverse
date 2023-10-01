library(highcharter)
library(ojodb)
hc_theme_ojo <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = ojo_pal,
    plotOptions = list(
      line = list(
        lineWidth = 3,
        marker = list(
          enabled = TRUE,
          lineColor = "#000000",
          lineWidth = 1.5
        )
      )
    ),
    legend = list(
      align = "center",
      verticalAlign = "top"
    )
  )
)
