#' Internal method.
#' 
#' @param likert object of type likert.
#' @param low.color color for low values.
#' @param high.color color for high values.
#' @param text.size size of text attributes.
#' @param text.color color of text attributes.
#' @param wrap width to wrap label text for non-grouped likert objects.
#' @param ... currently unused.
#' @seealso plot.likert
#' @seealso likert.bar.plot
#' @export
likert.heat.plot <- function(likert,
							 low.color='white',
							 high.color='blue', 
							 text.color='black',
							 text.size=4,
							 wrap=50,
							 ...) {
	if(!is.null(likert$grouping)) {
		stop('heat plots with grouping are not supported.')
	}
	
	lsum <- summary(likert)
	results = reshape2::melt(likert$results, id.vars='Item')
	results$variable = as.character(results$variable)
	results$label = paste(format(results$value, digits=2, drop0trailing=FALSE), '%', sep='')
	tmp = data.frame(Item=lsum$Item, 
					 variable=rep('Mean (SD)', nrow(lsum)),
					 value=rep(-100, nrow(lsum)),
					 label=paste(format(lsum$mean, digits=3, drop0trailing=FALSE), 
					 			' (', format(lsum$sd, digits=2, drop0trailing=FALSE), 
					 			')', sep=''),
					 stringsAsFactors=FALSE)
	results = rbind(tmp, results)
	
	p = ggplot(results, aes(x=Item, y=variable, fill=value, label=label)) + 
		scale_y_discrete(limits=c('Mean (SD)', names(likert$results)[2:ncol(likert$results)]) ) +
		geom_tile() + geom_text(size=text.size, colour=text.color) + coord_flip() + 
		scale_fill_gradient2("Percent", low='white', mid=low.color, high=high.color, limits=c(0,100)) + 
		xlab('') + ylab('') + 
		theme(panel.grid.major=element_blank(), 
			 panel.grid.minor=element_blank(), 
			 axis.ticks=element_blank(),
			 panel.background=element_blank()) +
		scale_x_discrete(breaks=likert$results$Item,
			labels=label_wrap_mod(likert$results$Item, width=wrap))
	class(p) <- c('likert.heat.plot', class(p))
	return(p)
}

#' Print method for \code{\link{likert.heat.plot}}.
#' @param p a plot from \code{\link{likert.heat.plot}}.
#' @param ... other parameters passed to ggplot2.
print.likert.heat.plot <- function(p, ...) {
	NextMethod(p, ...)
}
