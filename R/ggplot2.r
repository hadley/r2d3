#' @import rjson
#' @export
json <- function(x, ...) toJSON(compact_rec(as.list(x, ...)))

#' @S3method as.list proto
as.list.proto <- function(x, ...) x$as.list(...)

# @examples
# as.list(scale_x_continuous())
# json(scale_x_continuous())
#' @S3method as.list scale
as.list.scale <- function(x, ...) {
  x$aesthetics <- x$aesthetics[1]
  x$domain <- x$range$range
  x$scale_name <- NULL
  x$class <- rev(class(x))
  
  # Also need to deal with functions (e.g. oob, trans, rescaler, ...)
  # Not so important for the first round, trans especially is important
  # for full plot specification
  
  vector_only(x)
}

#' @S3method as.list facet
as.list.facet <- function(x, ...) {
  # Don't support facetting until I can figure out what the scales
  # data structure should look like - probably should be nested list.
  if (!inherits(x, "null")) {
    stop("Facetting not currently supported", call. = FALSE)
  }
  
  x$class <- rev(class(x))
  x$facets <- as.character(x$facets)
  vector_only(x)  
}

#' @S3method as.list coord
as.list.coord <- function(x, ...) {
  x$class <- rev(class(x))
  vector_only(x)  
}

# At what point should I combine the aesthetics from the plot and the layer?
# Or should this be something that the presentation layer can do?
#' @importFrom digest digest
as.list.layer <- function(., ...) {
  list(
    geom = c(list(name = .$geom$objname), .$geom_params),
    stat = c(list(name = .$stat$objname), .$stat_params),
    adj  = as.list.position(.$position),
    mapping = as.character(.$mapping),
    data = if (!is.null(.$data)) digest(.$data),
    show_guide = .$show_guide
  )
}

as.list.position <- function(., ...) {
  compact_rec(list(name = .$objname, width = .$width, height = .$height))
}

#' @S3method as.list ggplot
as.list.ggplot <- function(x, ...) {
  built <- ggplot_build(x)
  
  # For now, assuming no facetting (facet_null)
  x_range <- name_matches(built$panel$ranges[[1]], "^x\\.")
  y_range <- name_matches(built$panel$ranges[[1]], "^y\\.")
  x_scale <- modifyList(as.list(built$panel$x_scales[[1]]), x_range)
  y_scale <- modifyList(as.list(built$panel$y_scales[[1]]), y_range)
  # All other non-position scales
  np_scales <- lapply(built$scales$scales, as.list, ...)
  aesthetics <- vapply(np_scales, function(x) x$aesthetic[[1]], character(1))
  names(np_scales) <- aesthetics

  # Combine layers and data
  data <- built$data
  names(data) <- vapply(data, digest, character(1))
  layers <- lapply(x$layers, as.list.layer, ...)
  
  stopifnot(length(layers) == length(data))
  for (i in seq_along(data)) {
    layers[[i]]$data <- names(data)[[i]]
  }

  list(
    layers = layers,
    scales = c(list(x = x_scale, y = y_scale), np_scales),
    facet = as.list(x$facet, ...),
    coord = as.list(x$coord, ...),
    mapping = as.character(x$mapping),
    data = data
  )
}
