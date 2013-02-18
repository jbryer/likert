#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to \code{\link{likert.bar.plot}} or 
#'        \code{\link{likert.heat.plot}}.
#' @export
#' @seealso likert.bar.plot
#' @seealso likert.heat.plot
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, type=c('bar','heat'), ...) {
	if(type[1] == 'bar') {
		likert.bar.plot(likert, ...)
	} else {
		likert.heat.plot(likert, ...)
	}
}
