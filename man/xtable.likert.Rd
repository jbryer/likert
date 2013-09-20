\name{xtable.likert}
\alias{xtable.likert}
\title{Prints a LaTeX table of the likert items.}
\usage{
  \method{xtable}{likert} (x, caption = NULL, label = NULL,
    align = NULL, digits = NULL, display = NULL,
    include.n = TRUE, include.mean = TRUE,
    include.sd = TRUE, include.low = TRUE,
    include.neutral = (x$nlevels\%\%2 != 0),
    include.high = TRUE, include.levels = TRUE,
    include.missing = TRUE, center = (x$nlevels - 1)/2 + 1,
    ordered = TRUE, ...)
}
\arguments{
  \item{x}{likert class object.}

  \item{caption}{the table caption.}

  \item{label}{the table label.}

  \item{align}{column alignments.}

  \item{digits}{number of digits to use for numeric
  columns.}

  \item{display}{column formats.}

  \item{include.n}{option to include n}

  \item{include.mean}{option to include mean}

  \item{include.sd}{option to include sd}

  \item{include.low}{option to include low}

  \item{include.neutral}{option to include neutral}

  \item{include.high}{option to include high}

  \item{include.levels}{option to include levels}

  \item{include.missing}{option to include missing levels.}

  \item{center}{specifies which level should be treated as
  the center. For example, \code{center = 3} would use the
  third level as the center whereas \code{center = 3.5}
  would indicate no specific level is the center but <= 3
  are low levels and >= 4 are high levels (i.e. used for
  forced choice items or those without a neutral option).
  This also influences which levels are summarized in the
  low and high groups.}

  \item{ordered}{whether the results should be ordered. See
  \code{\link{summary.likert}}}

  \item{...}{other parameters passed to \link{xtable}.}
}
\description{
  Crate a LaTeX or HTML table of the \code{\link{likert}}
  results.
}
\seealso{
  \link{xtable}, \link{print.xtable}
}

