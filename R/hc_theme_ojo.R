#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[highcharter]{hc_theme_merge}}, \code{\link[highcharter]{hc_theme_smpl}}, \code{\link[highcharter]{hc_theme}}
#'  \code{\link[ojodb]{ojo_pal}}
#' @rdname hc_theme_ojo
#' @export
#' @author Anthony Flores
#' @importFrom highcharter hc_theme_merge hc_theme_smpl hc_theme
#' @importFrom ojodb ojo_pal
hc_theme_ojo <- function() {
  theme <- highcharter::hc_theme_merge(
    highcharter::hc_theme_smpl(),
    highcharter::hc_theme(
      colors = ojodb::ojo_pal,
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

  return(theme)
}
