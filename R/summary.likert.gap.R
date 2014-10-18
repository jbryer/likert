#' Prints summary table of a Likert analysis.
#'
#' The \code{summary} function returns a data frame that provides additional 
#' information. It contains 'Item' and 'Group' columns similiar to the results data
#' frame as well as a column 'low' corresponding to the sum of levels below
#' neutral, a column 'high' corresponding to the sum of levels above
#' neutral, and columns 'mean' and 'sd' corresponding to the mean and
#' standard deviation, respectively, of the results. The numeric values
#' are determined by as.numeric which will use the values of the factors.
#'          
#' @param object the likert class to summarize.
#' @param ... parameters passed to \code{\link{summary.likert}}
#' @return a list with two data frames with summarized data for satisfaction
#'         and importance results separately.
#' @export
#' @method summary likert.gap
summary.likert.gap <- function(object, ...) {
	return(list(
		Satisfaction=summary.likert(object, ...),
		Importance=summary.likert(object$importance, ...)
	))
}

