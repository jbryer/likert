\name{likert}
\alias{likert}
\title{Constructor function to create a likert class.}
\usage{
  likert(items, grouping = NULL,
    nlevels = length(levels(items[, 1])))
}
\arguments{
  \item{items}{data frame containing the likert based
  items. The variables in the data frame should be
  factors.}

  \item{grouping}{(optional) should the results be
  summarized by the given grouping variable.}

  \item{nlevels}{number of possible levels. Only necessary
  if there are missing levels.}
}
\value{
  a likert class with the following elements: results,
  items, grouping, nlevels, and summary.
}
\description{
  This function will provide various summary statistics
  about a set of likert items. The resulting object will
  have the following items:
}
\details{
  \itemize{ \item results this data frame will contain a
  column 'Item', 'Group' (if a grouping variable was
  specified, and a column for each level of the items (e.g.
  agree, disagree, etc.). The value within each cell
  corresponds to the percentage of responses for that level
  and group.  \item items a copy of the original items data
  frame.  \item grouping a copy of the original grouping
  vector.  \item nlevels the number of levels used in the
  calculations.  \item summary this data frame provides
  additional summary information. It will contain 'Item'
  and 'Group' columns similiar to the results data frame as
  well as a column 'low' corresponding to the sum of levels
  below neutral, a column 'high' corresponding to the sum
  of levels above neutral, and columns 'mean' and 'sd'
  corresponding to the mean and standard deviation,
  respectively, of the results. The numeric values are
  determined by as.numeric which will use the values of the
  factors. }
}

