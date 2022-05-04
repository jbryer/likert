#' Prints results table.
#'
#' @param x the likert class to print.
#' @param ... parameters passed to \code{\link{print.data.frame}}.
#' @export
#' @method print likert
print.likert <- function(x, ...) {
	print(x$results, ...)
}
