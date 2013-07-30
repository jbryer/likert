\name{reverse.levels}
\alias{reverse.levels}
\title{Reverse the levels of a factor.}
\usage{
  reverse.levels(x)
}
\arguments{
  \item{x}{a factor or a data.frame of factors whose levels
  will be reverse coded.}
}
\description{
  Reverse the levels of a factor.
}
\examples{
mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
test <- factor(sample(mylevels[1:5], 10, replace=TRUE))
cbind(test, as.integer(test), as.integer(reverse.levels(test)))
}

