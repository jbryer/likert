#' Plots a set of likert items. 
#'
#' @param x the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to \code{\link{likert.bar.plot}} or 
#'        \code{\link{likert.heat.plot}}.
#' @export
#' @seealso likert.bar.plot
#' @seealso likert.heat.plot
#' @seealso likert.density.plot
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(x, type=c('bar','heat','density'), ...) {
	if(type[1] == 'bar') {
		likert.bar.plot(x, ...)
	} else if(type[1] == 'heat') {
		likert.heat.plot(x, ...)
	} else if(type[1] == 'density') {
		likert.density.plot(x, ...)
	} else {
		stop('Invalid plot type specified.')
	}
}
