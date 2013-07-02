\name{likert.bar.plot}
\alias{likert.bar.plot}
\title{Bar Plot for Likert Items.}
\usage{
  likert.bar.plot(likert, low.color = "#D8B365",
    high.color = "#5AB4AC", neutral.color = "grey90",
    neutral.color.ramp = "white", text.size = 3,
    text.color = "black", centered = FALSE,
    center = (likert$nlevels - 1)/2 + 1,
    include.center = TRUE, ordered = TRUE,
    wrap = ifelse(is.null(likert$grouping), 50, 100),
    legend = "Response", ...)
}
\arguments{
  \item{likert}{object of type likert.}

  \item{low.color}{color for low values.}

  \item{high.color}{color for high values.}

  \item{neutral.color}{color for middle values (if odd
  number of levels).}

  \item{neutral.color.ramp}{second color used when calling
  \code{\link{colorRamp}} with \code{low.color} and
  \code{high.color} to define the color palettes.}

  \item{text.size}{size of text attributes.}

  \item{text.color}{color of text attributes.}

  \item{centered}{if true, the bar plot will be centered
  around zero such that the lower half of levels will be
  negative.}

  \item{center}{specifies which level should be treated as
  the center. For example, \code{center = 3} would use the
  third level as the center whereas \code{center = 3.5}
  would indicate no specific level is the center but <= 3
  are low levels and >= 4 are high levels (i.e. used for
  forced choice items or those without a neutral option).
  This also influences where the color breaks from low to
  high.}

  \item{include.center}{if TRUE, include the center level
  in the plot otherwise the center will be excluded.}

  \item{ordered}{reorder items from high to low.}

  \item{wrap}{width to wrap label text for non-grouped
  likert objects.}

  \item{legend}{title for the legend.}

  \item{...}{currently unused.}
}
\description{
  Bar plot for the results of \code{\link{likert}}.
}
\seealso{
  plot.likert

  likert.heat.plot
}

