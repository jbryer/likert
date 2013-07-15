#' Absolute value formatter for \code{\link{continuous_scale}}.
#' 
#' This will print the absolute value for labeling on axis. Usefull for stacked
#' bar plots where negative values are not negative percentages but represent
#' negative groups.
#' 
#' @param x value to be reformatted.
abs_formatter <- function(x) {
	return(abs(x))
}
