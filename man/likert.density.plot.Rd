\name{likert.density.plot}
\alias{likert.density.plot}
\title{Creates a density plot for likert items.}
\usage{
  likert.density.plot(likert, facet = TRUE, bw = 0.5,
    legend, ...)
}
\arguments{
  \item{likert}{object of type likert.}

  \item{facet}{for non-grouped items, should each density
  distribution be plotted in a separate facet.}

  \item{bw}{the smoothing bandwidth. This is often set to
  the standard deviation but this is often inadequate for
  Likert type items. The value of 0.5 is used since the
  difference between any two adjacent levels is one.}

  \item{legend}{title for the legend.}

  \item{...}{parameters passed to \code{\link{density}}.}
}
\description{
  This funciton will create a visualization that treats the
  likert items as a continuous variable.
}
\seealso{
  plot.likert
}

