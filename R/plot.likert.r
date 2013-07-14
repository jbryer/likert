#' Plots a set of likert items.
#' 
#' This is an implementation of the S3 plot generic function. Based upon the
#' \code{type} parameter this function will call either \code{\link{likert.bar.plot}},
#' \code{\link{likert.heat.plot}}, or \code{\link{likert.density.plot}}. See the
#' help pages for those functions for all the available parameters to customize
#' the aesthetics of the figure. Although those functions can be plotted directly,
#' we recommend call the generic \code{plot} function.
#'
#' @param x the likert items to plot
#' @param type the type of plot to create. Current values are bar and heat.
#' @param ... other parameters passed passed to \code{\link{likert.bar.plot}} or 
#'        \code{\link{likert.heat.plot}}.
#' @param panel.background define background of the plot. See \code{\link{theme}}.
#' @export
#' @seealso likert.bar.plot
#' @seealso likert.heat.plot
#' @seealso likert.density.plot
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(x, type=c('bar','heat','density'),
						panel.background=element_rect(size=1, color='grey70', fill=NA),
						...) {
	if(type[1] == 'bar') {
		likert.bar.plot(x, ...) + 
			theme(panel.background=element_rect(size=1, color='grey70', fill=NA))
	} else if(type[1] == 'heat') {
		likert.heat.plot(x, ...) +
			theme(panel.background=element_rect(size=1, color='grey70', fill=NA))
	} else if(type[1] == 'density') {
		likert.density.plot(x, ...) +
			theme(panel.background=element_rect(size=1, color='grey70', fill=NA))
	} else {
		stop('Invalid plot type specified.')
	}
}
