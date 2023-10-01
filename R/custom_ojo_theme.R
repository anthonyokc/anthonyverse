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
#'  \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{margin}}
#' @rdname custom_ojo_theme
#' @export 
#' @author Anthony Flores
#' @importFrom ggplot2 theme_bw theme element_blank element_rect element_text
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
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
