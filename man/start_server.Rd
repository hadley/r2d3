\name{start_server}
\alias{start_server}
\title{Start server for r2d3 files.}
\usage{
  start_server(base, appname = "r2d3",
    browse = interactive())
}
\arguments{
  \item{base}{A directory specifying the base path where
  files are looked for.}

  \item{appname}{The name of the application - this is only
  needed if you want to server multiple r2d3 servers out of
  different directories.}

  \item{browse}{if \code{TRUE} will open the server in the
  browser}
}
\value{
  (invisibly) the Rook server.
}
\description{
  This isn't absolutely necessary, but if you run the html
  from a \code{file://} you can't request other local
  files. This server also provides some convenience
  features like:
}
\details{
  \itemize{ \item If the file doesn't exist in the base
  directory, we'll look in the installed package directory.
  That way you don't need to worry about having
  \code{d3.js} etc installed.

  \item If a html file doesn't exist, a template will be
  rendered that runs the r2d3 js using a json file of the
  matching name. }
}
\examples{
start_server(system.file("examples", package = "r2d3"))
}

