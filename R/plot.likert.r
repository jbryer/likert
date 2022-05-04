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
#' @param include.histogram if TRUE, a histogram of count of responses is also plotted.
#' @param panel.widths if \code{include.histogram=TRUE}, this vector of length two
#'        specifies the ratio of the left and right panels.
#' @param legend.position the position for the legend ("left", "right", "bottom",
#'        "top", or two-element numeric vector).
#' @param panel.arrange how panels for grouped likert items should be arrange.
#'        Possible values are \code{v} (vertical, the default), \code{h}
#'        (horizontal), and \code{NULL} (auto fill horizontal and vertical)
#' @param panel.strip.color the background color for panel labels.
#' @param group.order the order in which groups (for grouped items) or items
#'        (for non-grouped items) should be plotted.
#' @export
#' @seealso \link{likert.bar.plot}
#' @seealso \link{likert.heat.plot}
#' @seealso \link{likert.density.plot}
#' @seealso \link{likert.histogram.plot}
#' @method plot likert
plot.likert <- function(x, type=c('bar','heat','density'),
						include.histogram=FALSE,
						panel.widths=c(3,1),
						panel.arrange='v',
						panel.strip.color='#F0F0F0',
						legend.position='bottom',
						group.order,
						panel.background=element_rect(size=1, color='grey70', fill=NA),
						...) {
	if(missing(group.order) & !is.null(x$grouping) & include.histogram) {
		# Fixes this issue: https://github.com/jbryer/likert/issues/40
		# If the order is not explicitly set when plotting histograms too, then
		# the groups may not align.
		group.order <- unique(x$grouping)
	}
	if(type[1] == 'bar') {
		p <- likert.bar.plot(x, 
							 legend.position=legend.position, 
							 panel.arrange=panel.arrange,
							 panel.strip.color=panel.strip.color,
							 group.order = group.order,
							 ...)
	} else if(type[1] == 'heat') {
		p <- likert.heat.plot(x, legend.position=legend.position, ...)
	} else if(type[1] == 'density') {
		p <- likert.density.plot(x, panel.arrange=panel.arrange,
								 panel.strip.color=panel.strip.color,
								 legend.position=legend.position,
								 ...)
	} else {
		stop('Invalid plot type specified.')
	}
	p <- p + theme(panel.background=panel.background)

	if(include.histogram) {
		if(type[1] == 'bar') {
			item.order <- attr(p, 'item.order')
			phist <- likert.histogram.plot(x, 
										   legend.position=legend.position, 
										   order=item.order,
										   panel.arrange=panel.arrange,
										   panel.strip.color=panel.strip.color,
										   group.order=group.order,
										   ...)
	 		phist <- phist + theme(panel.background=panel.background)
			if(panel.arrange == 'v') {
				phist <- phist + theme(axis.text.y=element_blank())
				#phist <- phist + theme(strip.text=element_text(size=0))
				grid_layout <- grid.layout(nrow=1, ncol=2, widths=panel.widths)
				grid.newpage()
				pushViewport( viewport( layout=grid_layout ) )
				suppressWarnings({ #HACK to remove "Stacking not well defined when ymin != 0"
					align.plots(grid_layout, 
								list(p, 1, 1), 
								list(phist, 1, 2))
				})			
			} else if(panel.arrange == 'h') {
				grid_layout <- grid.layout(nrow=2, ncol=1, heights=panel.widths)
				grid.newpage()
				pushViewport( viewport( layout=grid_layout ) )
				suppressWarnings({ #HACK to remove "Stacking not well defined when ymin != 0"
					align.plots(grid_layout, 
								list(p, 1, 1), 
								list(phist, 2, 1))
				})			
			} else {
				stop(paste0('panel.arrange of ', panel.arrange, ' not defined with ',
							'include.histogram=TRUE'))
			}
		} else {
			warning('Plotting histogram is only supported with type="bar" plots.')
			return(p)
		}
	} else {
		return(p)
	}
}
