#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to \code{\link{plot.likert.bar}} or 
#'        \code{\link{plot.likert.heat}}.
#' @export
#' @seealso plot.likert.bar
#' @seealso plot.likert.heat
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, type=c('bar','heat'), ...) {
	if(type[1] == 'bar') {
		plot.likert.bar(likert, ...)
	} else {
		plot.likert.heat(likert, ...)
	}
}
