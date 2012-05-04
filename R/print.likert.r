#' Prints results table.
#'
#' @param x the likert class to print.
#' @export
#' @method print likert
#' @S3method print likert
print.likert <- function(x, ...) {
	print(x$results)
}
