#' Start server for r2d3 files.
#' 
#' This isn't absolutely necessary, but if you run the html from a
#' \code{file://} you can't request other local files. This server also
#' provides some convenience features like:
#'
#' \itemize{
#'  \item If the file doesn't exist in the base directory, we'll look in 
#'    the installed package directory. That way you don't need to worry
#'    about having \code{d3.js} etc installed.
#'
#'  \item If a html file doesn't exist, a template will be rendered that
#'    runs the r2d3 js using a json file of the matching name.
#' }
#' @param base A directory specifying the base path where files are looked
#'   for.
#' @param appname The name of the application - this is only needed if you
#'  want to server multiple r2d3 servers out of different directories.
#' @param browse if \code{TRUE} will open the server in the browser
#' @return (invisibly) the Rook server.
#' @export
#' @import Rook
#' @importFrom tools file_path_sans_ext
#' @examples
#' start_server(system.file("examples", package = "r2d3"))
start_server <- function(base, appname = "r2d3", browse = interactive()) {
  stopifnot(file.exists(base), file.info(base)$isdir)
  base <- normalizePath(base)
  
  server <- Rhttpd$new()
  server$add(make_router(base), appname)
  server$start(quiet = TRUE)
  
  if (browse) server$browse(appname)
  invisible(server)
}

make_router <- function(base) {
  function(env) {
    req <- Request$new(env)
    path <- req$path_info()
    
    # Found in base directory, so serve it from there
    base_path <- file.path(base, path)
    if (file.exists(base_path)) {
      if (file.info(base_path)$isdir) {
        if (grepl("/$", path)) {
          # It's a directory, so make a basic index
          return(serve_index(base_path))          
        } else {
          return(redirect(paste0(req$path(), "/")))
        }
      } else {
        return(serve_file(base_path))
      }
    }

    # Found in installed path, so serve it from there
    installed_path <- file.path(inst_path(), path)
    if (file.exists(installed_path)) return(serve_file(installed_path))

    # If it's an html file, and a json file with the same name exists,
    # serve the standard template
    json_path <- paste0(file_path_sans_ext(path), ".json")
    if (file.exists(file.path(base, json_path))) {
      return(serve_scaffold(paste0(req$script_name(), json_path)))
    }
    
    # Couldn't find it, so return 404.
    res <- Response$new(status = 404L)
    res$header("Content-type", "text/plain")
    res$write(paste("Couldn't find path:", path))
    res$finish()
  }
}

#' @importFrom tools file_ext
serve_file <- function(path) {
  stopifnot(file.exists(path))
  
  fi <- file.info(path)
  body <- readBin(path, 'raw', fi$size)
  
  res <- Response$new()
  res$header("Content-Type", Mime$mime_type(paste0(".", file_ext(path))))
  res$write(rawToChar(body))
  res$finish()
}

#' @importFrom whisker whisker.render
serve_template <- function(template_path, data) {
  template <- readLines(file.path(inst_path(), template_path))
  body <- whisker.render(template, data)
  
  res <- Response$new()
  res$header("Content-Type", "text/html")
  res$write(body)
  res$finish()
}

serve_scaffold <- function(path) {
  serve_template("template.html", list(json_path = path))
}

serve_index <- function(path) {
  files <- basename(dir(path))
  serve_template("index.html", list(files = files))
}

redirect <- function(path) {
  res <- Response$new(status = 301L)
  res$header("Location", path)
  res$finish()
}