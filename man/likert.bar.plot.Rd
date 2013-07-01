\name{likert.bar.plot}
\alias{likert.bar.plot}
\title{Bar Plot for Likert Items.}
\usage{
  likert.bar.plot(likert, low.color = "blue",
    high.color = "red", neutral.color = "white",
    text.size = 3, text.color = "black", centered = FALSE,
    ordered = TRUE, wrap = 50, ...)
}
\arguments{
  \item{likert}{object of type likert.}

  \item{low.color}{color for low values.}

  \item{high.color}{color for high values.}

  \item{neutral.color}{color for middle values (if odd
  number of levels).}

  \item{text.size}{size of text attributes.}

  \item{text.color}{color of text attributes.}

  \item{centered}{if true, the bar plot will be centered
  around zero such that the lower half of levels will be
  negative.}

  \item{ordered}{reorder items from high to low.}

  \item{wrap}{width to wrap label text for non-grouped
  likert objects.}

  \item{...}{currently unused.}
}
\description{
  Bar plot for the results of \code{\link{likert}}.
}
\seealso{
  plot.likert

  likert.heat.plot
}

