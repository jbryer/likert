\name{recode}
\alias{recode}
\title{Recode a vector.}
\usage{
  recode(x, from, to, to.class = NULL)
}
\arguments{
  \item{x}{the vector whose values will be recoded.}

  \item{from}{the old values in x to be recoded.}

  \item{to}{the new values.}

  \item{to.class}{an 'as.' function representing the
  desired vector type (i.e.  as.character, as.numeric,
  as.logical, as.numeric).}
}
\value{
  a vector with same length of x with recoded values.
}
\description{
  Recode a vector.
}

