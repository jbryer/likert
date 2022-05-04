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
#' @param legend title for the legend.
#' @param satisfaction.label label used for satisfaction items.
#' @param importance.label label used for importance items.
#' @export
#' @seealso \link{likert.bar.plot}
#' @seealso \link{likert.heat.plot}
#' @seealso \link{likert.density.plot}
#' @seealso \link{likert.histogram.plot}
#' @method plot likert.gap
plot.likert.gap <- function(x, type=c('bar','density'),
						include.histogram=FALSE,
						panel.widths=c(3,1),
						panel.arrange='v',
						panel.strip.color='#F0F0F0',
						legend.position='bottom',
						panel.background=element_rect(size=1, color='grey70', fill=NA),
						satisfaction.label='Satisfaction',
						importance.label='Importance',
						legend,
						...) {
	if(type[1] == 'bar') {
		stop('Not implemented yet! It is coming')
	} else if(type[1] == 'heat') {
		warning('Heat plot not supported for gap analysis, will ignore importance ratings.')
		return(NextMethod(x, type=type, include.histogram=include.histogram,
				   panel.widths=panel.widths,
				   panel.arrange=panel.arrange,
				   panel.strip.color=panel.strip.color,
				   legend.position=legend.position,
				   panel.background=panel.background, ...))
	} else if(type[1] == 'density') {
		sat <- x$items
		imp <- x$importance$items
		sat$Type <- satisfaction.label
		imp$Type <- importance.label
		sat$Row <- 1:nrow(sat)
		imp$Row <- 1:nrow(imp)
		df <- rbind(sat, imp)
		df.melt <- reshape2::melt(df, id.vars=c('Row','Type'))
		df.melt$value <- as.integer(df.melt$value)
		names(df.melt) <- c('Row','Type','Question','Value')
		tab <- describeBy(df.melt$Value, group=list(df.melt$Type, df.melt$Question),
						  mat=TRUE, skew=FALSE)[,c('group1','group2','mean')]
		names(tab) <- c('Type','Question','Mean')
		p <- ggplot(df.melt, aes(x=Value, group=Type, color=Type)) + 
			geom_density() + 
			geom_vline(data=tab, aes(xintercept=Mean, color=Type)) +
			facet_wrap(~ Question, ncol=1) +
			xlab('') + ylab('')
		if(!missing(legend)) {
			p <- p + labs(fill=legend, color=legend)
		}
	} else {
		stop('Invalid plot type specified.')
	}
	p <- p + theme(panel.background=panel.background)

	#TODO: Someday!
# 	if(include.histogram) {
# 		if(type[1] == 'bar') {
# 			item.order <- attr(p, 'item.order')
# 			phist <- likert.histogram.plot(x, 
# 										   legend.position=legend.position, 
# 										   order=item.order,
# 										   panel.arrange=panel.arrange,
# 										   panel.strip.color=panel.strip.color,
# 										   ...)
# 	 		phist <- phist + theme(panel.background=panel.background)
# 			if(panel.arrange == 'v') {
# 				phist <- phist + theme(axis.text.y=element_blank())
# 				#phist <- phist + theme(strip.text=element_text(size=0))
# 				grid_layout <- grid.layout(nrow=1, ncol=2, widths=panel.widths)
# 				grid.newpage()
# 				pushViewport( viewport( layout=grid_layout ) )
# 				suppressWarnings({ #HACK to remove "Stacking not well defined when ymin != 0"
# 					align.plots(grid_layout, 
# 								list(p, 1, 1), 
# 								list(phist, 1, 2))
# 				})			
# 			} else if(panel.arrange == 'h') {
# 				grid_layout <- grid.layout(nrow=2, ncol=1, heights=panel.widths)
# 				grid.newpage()
# 				pushViewport( viewport( layout=grid_layout ) )
# 				suppressWarnings({ #HACK to remove "Stacking not well defined when ymin != 0"
# 					align.plots(grid_layout, 
# 								list(p, 1, 1), 
# 								list(phist, 2, 1))
# 				})			
# 			} else {
# 				stop(paste0('panel.arrange of ', panel.arrange, ' not defined with ',
# 							'include.histogram=TRUE'))
# 			}
# 		} else {
# 			warning('Plotting histogram is only supported with type="bar" plots.')
# 			return(p)
# 		}
# 	} else {
# 		return(p)
# 	}
	return(p)
}
