#' Prints summary table.
#'
#' @param x the likert class to summarize.
#' @export
#' @method summary likert
#' @S3method summary likert
summary.likert <- function(x, ...) {
	return(x$summary)
}
