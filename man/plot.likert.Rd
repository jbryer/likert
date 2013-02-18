\name{plot.likert}
\alias{plot.likert}
\title{Plots a set of likert items.}
\usage{
  \method{plot}{likert} (likert, type = c("bar", "heat"),
    ...)
}
\arguments{
  \item{likert}{the likert items to plot}

  \item{type}{the type of plot to create. Current values
  are bar and heat.}

  \item{...}{other parameters passed passed to
  \code{\link{plot.likert.bar}} or
  \code{\link{plot.likert.heat}}.}
}
\description{
  Plots a set of likert items.
}
\seealso{
  plot.likert.bar

  plot.likert.heat
}

