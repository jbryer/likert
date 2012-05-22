#' Internal method.
#' @param centered if true, the bar plot will be centered around zero such that
#'        the lower half of levels will be negative.
#' @seealso plot.likert
plot.likert.bar <- function(likert, low.color='blue', high.color='red', 
							neutral.color='white', text.size=3, text.color='black',
							centered=FALSE, ordered=TRUE, ...) {
	lowrange = 1 : ceiling(likert$nlevels / 2 - likert$nlevels %% 2)
	highrange = ceiling(likert$nlevels / 2 + 1 ) : likert$nlevels
	ramp = colorRamp(c(low.color, neutral.color))
	ramp = rgb( ramp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
	bamp = colorRamp(c(neutral.color, high.color))
	bamp = rgb( bamp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
	cols = NULL
	if(likert$nlevels %% 2 != 0) {
		cols = c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
	} else {
		cols = c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])
	}
	
	if(!is.null(likert$grouping)) {
		results = melt(likert$results, id=c('Group', 'Item'))
		ymin = 0
		p = ggplot(results, aes(y=value, x=Group, group=variable))
		if(centered) {
			ymin = -100
			rows = which(results$variable %in% names(likert$results)[
				3:( ncol(likert$results) / 2 + 1 )])
			results[rows, 'value'] = -1 * results[rows, 'value']
			results.low = results[rows,]
			results.high = results[-rows,]
			p = p + 
				geom_bar(data=results.low, aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-100,100))
		} else {
			p = p + geom_bar(stat='identity', aes(fill=variable)) + ylim(c(-5,105))
		}
		p = p + #ggplot(results, aes(y=value, x=Group, group=variable)) + 
			scale_fill_manual('Response', values=cols, 
							  breaks=unique(results$variable),
							  labels=levels(likert$items[,i])) + 
		  	geom_text(data=likert$summary, y=ymin, aes(x=Group, 
  					label=paste(round(low), '%', sep=''), group=Item), 
		  			size=text.size, hjust=1) +
			geom_text(data=likert$summary, aes(x=Group, y=100, 
					label=paste(round(high), '%', sep=''), 
					group=Item), size=text.size, hjust=-.2) +
			coord_flip() +	ylab('Percentage') + xlab('') + 
			opts(axis.ticks=theme_blank()) + facet_wrap(~ Item, ncol=1)
	} else {
		results = melt(likert$results, id.vars='Item')
		if(ordered) {
			order = likert$summary[order(likert$summary$high),'Item']
			results$Item = factor(results$Item, levels=order)
		}
		p = ggplot(results, aes(y=value, x=Item, group=Item))
		ymin = 0
		if(centered) {
			ymin = -100
			rows = which(results$variable %in% names(likert$results)[1:(
				(ncol(likert$results)-1)/2)])
			results[rows, 'value'] = -1 * results[rows, 'value']
			results.low = results[rows,]
			results.high = results[-rows,]
			p = p + 
				geom_bar(data=results.low, aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-100,100))
		} else {
			p = p + geom_bar(stat='identity', aes(fill=variable)) + ylim(c(-5,105))
		}
		p = p + 
			scale_fill_manual('Response', values=cols, 
							  breaks=unique(results$variable), 
							  labels=levels(likert$items[,i])) + 
			geom_text(data=likert$summary, y=ymin, aes(x=Item, 
				  			label=paste(round(low), '%', sep='')), 
				  			size=text.size, hjust=1) +
			geom_text(data=likert$summary, y=100, aes(x=Item,
				  			label=paste(round(high), '%', sep='')), 
				  			size=text.size, hjust=-.2) +
			coord_flip() + ylab('Percentage') + xlab('') + 
			opts(axis.ticks=theme_blank())
	} 
	return(p)
}
