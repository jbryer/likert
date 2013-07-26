\name{likert.histogram.plot}
\alias{likert.histogram.plot}
\title{Histogram of number of responses.}
\usage{
  likert.histogram.plot(l, xlab = "n", plot.missing = TRUE,
    bar.color = "grey70", missing.bar.color = "maroon",
    label.completed = "Completed",
    label.missing = "Missing", legend.position = "bottom",
    wrap = ifelse(is.null(l$grouping), 50, 100), order,
    group.order, panel.arrange = "v",
    panel.strip.color = "#F0F0F0", ...)
}
\arguments{
  \item{l}{results of \code{\link{likert}}.}

  \item{xlab}{label used for the x-axis.}

  \item{plot.missing}{if TRUE, missing values will be
  plotted to the left of the x-axis.}

  \item{bar.color}{the bar color.}

  \item{missing.bar.color}{the color of the bar for missing
  values.}

  \item{label.completed}{the label to use in the legend
  representing the count of complete values.}

  \item{label.missing}{the label to use in the legend
  representing the count of missing values.}

  \item{order}{the order of the items.}

  \item{...}{other ggplot2 parameters.}

  \item{legend.position}{the position for the legend
  ("left", "right", "bottom", "top", or two-element numeric
  vector).}

  \item{wrap}{width to wrap label text for item labels}

  \item{group.order}{the order in which groups (for grouped
  items) or items (for non-grouped items) should be
  plotted.}

  \item{panel.arrange}{how panels for grouped likert items
  should be arrange.  Possible values are \code{v}
  (vertical, the default), \code{h} (horizontal), and
  \code{NULL} (auto fill horizontal and vertical)}

  \item{panel.strip.color}{the background color for panel
  labels.}
}
\description{
  Plots a histogram of the number of responses for each
  item and group (if specified). Negative values (in maroon
  by default) indicate the number of missing values for
  that item and group.
}

