#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to \code{\link{likert.bar.plot}} or 
#'        \code{\link{likert.heat.plot}}.
#' @export
#' @seealso likert.bar.plot
#' @seealso likert.heat.plot
#' @seealso likert.density.plot
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, type=c('bar','heat','density'), ...) {
	if(type[1] == 'bar') {
		likert.bar.plot(likert, ...)
	} else if(type[1] == 'heat') {
		likert.heat.plot(likert, ...)
	} else if(type[1] == 'density') {
		likert.density.plot(likert, ...)
	} else {
		stop('Invalid plot type specified.')
	}
}
