\name{r2d3}
\alias{r2d3}
\title{Render a ggplot2 graphic using d3.}
\usage{
  r2d3(x, path)
}
\arguments{
  \item{x}{plot}

  \item{path}{location to save json/html}
}
\description{
  Render a ggplot2 graphic using d3.
}
\examples{
p <- qplot(displ, hwy, data = mpg)
r2d3(p, "mpg.json")
}

