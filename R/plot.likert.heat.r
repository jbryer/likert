#' Internal method.
#' @seealso plot.likert
plot.likert.heat <- function(likert, low.color='white', high.color='blue', 
							 text.color='black', text.size=2, ...) {
	if(!is.null(likert$grouping)) {
		stop('likert plots with grouping are not supported.')
	}
	
	results = melt(likert$results, id.vars='Item')
	results$variable = as.character(results$variable)
	results$label = paste(format(results$value, digits=2, drop0trailing=FALSE), '%', sep='')
	tmp = data.frame(Item=likert$summary$Item, 
					 variable=rep('Mean (SD)', nrow(likert$summary)),
					 value=rep(-100, nrow(likert$summary)),
					 label=paste(format(likert$summary$mean, digits=3, drop0trailing=FALSE), 
					 			' (', format(likert$summary$sd, digits=2, drop0trailing=FALSE), 
					 			')', sep=''),
					 stringsAsFactors=FALSE)
	results = rbind(tmp, results)
	
	p = ggplot(results, aes(x=Item, y=variable, fill=value, label=label)) + 
		scale_y_discrete(limits=c('Mean (SD)', names(likert$results)[1:(ncol(likert$results)-1)]) ) +
		geom_tile() + geom_text(size=text.size, colour=text.color) + coord_flip() + 
		scale_fill_gradient2("Percent", low='white', mid=low.color, high=high.color, limits=c(0,100)) + 
		xlab('') + ylab('') + 
		opts(panel.grid.major=theme_blank(), 
			 panel.grid.minor=theme_blank(), 
			 axis.ticks=theme_blank(),
			 panel.background=theme_blank())
	return(p)
}
