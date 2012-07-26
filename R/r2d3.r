#' Render a ggplot2 graphic using d3.
#'
#' @param x plot
#' @param path location to save json/html
#' @export
#' @examples
#' p <- qplot(displ, hwy, data = mpg)
#' r2d3(p, "mpg.json")
r2d3 <- function(x, path) {
  # stopifnot(is.ggplot(x))
  
  cat(json(x), file = path)
}