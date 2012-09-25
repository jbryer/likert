\name{plot.likert.bar}
\alias{plot.likert.bar}
\title{Internal method.}
\usage{
  plot.likert.bar(likert, low.color = "blue",
    high.color = "red", neutral.color = "white",
    text.size = 3, text.color = "black", centered = FALSE,
    ordered = TRUE, ...)
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

  \item{...}{currently unused.}

  \item{ordered}{reorder items from high to low.}
}
\description{
  Internal method.
}
\seealso{
  plot.likert
}

