\name{plot.likert}
\alias{plot.likert}
\title{Plots a set of likert items.}
\usage{
  \method{plot}{likert} (x,
    type = c("bar", "heat", "density"),
    panel.background = element_rect(size = 1, color = "grey70", fill = NA),
    ...)
}
\arguments{
  \item{x}{the likert items to plot}

  \item{type}{the type of plot to create. Current values
  are bar and heat.}

  \item{...}{other parameters passed passed to
  \code{\link{likert.bar.plot}} or
  \code{\link{likert.heat.plot}}.}

  \item{panel.background}{define background of the plot.
  See \code{\link{theme}}.}
}
\description{
  Plots a set of likert items.
}
\seealso{
  likert.bar.plot

  likert.heat.plot

  likert.density.plot
}

