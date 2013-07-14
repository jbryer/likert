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
  This is an implementation of the S3 plot generic
  function. Based upon the \code{type} parameter this
  function will call either \code{\link{likert.bar.plot}},
  \code{\link{likert.heat.plot}}, or
  \code{\link{likert.density.plot}}. See the help pages for
  those functions for all the available parameters to
  customize the aesthetics of the figure. Although those
  functions can be plotted directly, we recommend call the
  generic \code{plot} function.
}
\seealso{
  likert.bar.plot

  likert.heat.plot

  likert.density.plot
}

