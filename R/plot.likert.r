#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to plot.likert.bar or plot.likert.heat
#' @export
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, type=c('bar','heat'), ...) {
	if(type[1] == 'bar') {
		plot.likert.bar(likert, ...)
	} else {
		plot.likert.heat(likert, ...)
	}
}
