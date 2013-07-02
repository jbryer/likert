#' Creates a density plot for likert items.
#' 
#' This funciton will create a visualization that treats the likert items as
#' a continuous variable.
#' 
#' @param likert object of type likert.
#' @param facet for non-grouped items, should each density distribution be plotted
#'        in a separate facet.
#' @param ... currently unused.
#' @export
#' @seealso plot.likert
likert.density.plot <- function(likert, facet=TRUE, ...) {
	lsum <- summary(likert)
	items <- likert$items
	items.density <- data.frame()
	labels <- likert:::label_wrap_mod(
		paste0(levels(items[,1]), ' (', 1:likert$nlevels, ')'), width=10)
	
	if(is.null(likert$grouping)) {
		for(l in seq_along(items)) {
			den <- density(as.integer(items[,l]), bw=0.5, na.rm=TRUE)
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
	} else {
		groups <- likert$grouping
		for(g in unique(groups)) {
			items.g <- items[groups == g,]
			for(l in seq_along(items)) {
				den <- density(as.integer(items.g[,l]), bw=0.5, na.rm=TRUE)
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
	
	return(p)
}
