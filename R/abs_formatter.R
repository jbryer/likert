#' Absolute value formatter for continuous_scale
#' 
#' This will print the absolute value for labeling on axis. Usefull for stacked
#' bar plots where negative values are not negative percentages but represent
#' negative groups.
#' 
#' @param x value to be reformatted.
#' @return the absolute value of x.
abs_formatter <- function(x) {
	return(abs(x))
}
