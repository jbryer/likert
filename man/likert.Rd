\name{likert}
\alias{likert}
\title{Analyze Likert type items.}
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
  This function will provide various statistics about a set
  of likert items. The resulting object will have the
  following items:
}
\details{
  \itemize{ \item \code{results} - this data frame will
  contain a column 'Item', 'Group' (if a grouping variable
  was specified, and a column for each level of the items
  (e.g. agree, disagree, etc.). The value within each cell
  corresponds to the percentage of responses for that level
  and group.  \item \code{items} - a copy of the original
  items data frame.  \item \code{grouping} - a copy of the
  original grouping vector.  \item \code{nlevels} - the
  number of levels used in the calculations. }
}
\examples{
data(pisaitems)
items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
names(items29) <- c("Magazines", "Comic books", "Fiction",
                   "Non-fiction books", "Newspapers")
l29 <- likert(items29)
summary(l29)
plot(l29)
}
\seealso{
  plot.likert

  summary.likert
}

