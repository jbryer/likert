#' Creates a density plot for likert items.
#' 
#' This funciton will create a visualization that treats the likert items as
#' a continuous variable.
#' 
#' @param likert object of type likert.
#' @param facet for non-grouped items, should each density distribution be plotted
#'        in a separate facet.
#' @param bw the smoothing bandwidth. This is often set to the standard deviation
#'        but this is often inadequate for Likert type items. The value of 0.5
#'        is used since the difference between any two adjacent levels is one.
#' @param legend title for the legend.
#' @param ... parameters passed to \code{\link{density}}.
#' @export
#' @seealso plot.likert
likert.density.plot <- function(likert, 
								facet=TRUE, 
								bw=0.5, 
								legend, 
								...) {
	lsum <- summary(likert)
	items <- likert$items
	items.density <- data.frame()
	labels <- label_wrap_mod(
		paste0(levels(items[,1]), ' (', 1:likert$nlevels, ')'), width=10)
	
	if(is.null(likert$grouping)) { #No Grouping
		for(l in seq_along(items)) {
			suppressWarnings({
				den <- density(as.integer(items[,l]), bw=bw, na.rm=TRUE, ...)
			})
			items.density <- rbind(items.density, 
								   data.frame(Item=names(items)[l], x=den$x, y=den$y))
		}
		
		p <- ggplot(items.density, aes(x=x, y=y, color=Item, fill=Item, group=Item)) + 
			geom_polygon(alpha=.05) + 
			geom_vline(data=lsum, aes(xintercept=mean, color=Item)) +
			geom_path() +
			#geom_rug(data=lsum, aes(x=mean, y=0), sides='b') +
			scale_x_continuous(breaks=1:likert$nlevels, labels=labels) +
			xlab('') + ylab('') +
			theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
		if(facet) {
			p <- p + facet_wrap(~ Item, ncol=1) + theme(legend.position='none')
		}
	} else { # Grouping
		groups <- likert$grouping
		for(g in unique(groups)) {
			items.g <- items[groups == g,]
			for(l in seq_along(items)) {
				suppressWarnings({
					den <- density(as.integer(items.g[,l]), bw=bw, na.rm=TRUE, ...)
				})
				items.density <- rbind(items.density, 
						data.frame(Item=names(items)[l], Group=g, x=den$x, y=den$y))
			}
		}
		
		p <- ggplot(items.density, aes(x=x, y=y, color=Group, fill=Group, group=Group)) + 
			geom_polygon(alpha=.05) + 
			geom_vline(data=lsum, aes(xintercept=mean, color=Group)) +
			geom_path() +
			#geom_rug(data=lsum, aes(x=mean, y=1, color=Group)) +
			facet_wrap(~ Item, ncol=1) +
			scale_x_continuous(breaks=1:likert$nlevels, labels=labels) +
			xlab('') + ylab('') +
			theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
	}
	
	if(!missing(legend)) {
		p <- p + labs(fill=legend, color=legend)
	}
	
	return(p)
}
