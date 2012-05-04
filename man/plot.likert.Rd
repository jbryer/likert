\name{plot.likert}
\alias{plot.likert}
\title{Plots a set of likert items.}
\usage{
  \method{plot}{likert} (likert, text.size = 2,
    type = c("bar", "heat"), ...)
}
\arguments{
  \item{likert}{the likert items to plot}

  \item{low.color}{color corresponding to the lowest value
  likert items}

  \item{high.color}{color corresponding to the highest
  value likert items}

  \item{neutral.color}{color for middle values. Only used
  when there are an odd number of levels.}

  \item{text.size}{size or text labels}

  \item{text.color}{the color of text in heat map cells}

  \item{type}{whether to plot a bar or heat map graphic}
}
\description{
  Plots a set of likert items.
}

