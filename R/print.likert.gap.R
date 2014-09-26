utils::globalVariables(c('Value','Type','Mean'))

#' Prints results table.
#'
#' @param x the likert class to print.
#' @param ... parameters passed to \code{\link{print.data.frame}}.
#' @export
#' @method print likert.gap
print.likert.gap <- function(x, ...) {
	cat('Satisfaction results:\n')
	print(x$results, ...)
	cat('\nImportance results:\n')
	print(x$importance$results, ...)
}
