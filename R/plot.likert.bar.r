#' Internal method.
#' 
#' @param likert object of type likert.
#' @param low.color color for low values.
#' @param high.color color for high values.
#' @param neutral.color color for middle values (if odd number of levels).
#' @param text.size size of text attributes.
#' @param text.color color of text attributes.
#' @param centered if true, the bar plot will be centered around zero such that
#'        the lower half of levels will be negative.
#' @param ... currently unused.
#' @param ordered reorder items from high to low.
#' @export
#' @seealso plot.likert
#' @seealso likert.heat.plot
likert.bar.plot <- function(likert, low.color='blue', high.color='red', 
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
	
	p <- NULL
	if(!is.null(likert$grouping)) {
		results = melt(likert$results, id=c('Group', 'Item'))
		results$variable <- factor(results$variable, ordered=TRUE)
		ymin = 0

		if(centered) {
			ymin = -100
			rows.low = which(results$variable %in% names(likert$results)[
				3:( ncol(likert$results) / 2 + 1 )])
			rows.high = which(results$variable %in% names(likert$results)[
				( ceiling(ncol(likert$results) / 2 + 2) ):ncol(likert$results)])
			results[rows.low, 'value'] = -1 * results[rows.low, 'value']
			results.low = results[rows.low,]
			results.high = results[rows.high,]
			if(likert$nlevels %% 2 != 0) {
				#Odd number of levels, will remove the middle
				#TODO: Perhaps have an option to leave it.
				rows.mid = which(results$variable %in% names(likert$results)[
					( floor(ncol(likert$results) / 2 + 2) )])
				results[rows.mid, 'value'] = 0
			}

			p = ggplot(results, aes(y=value, x=Group, group=variable)) + 
				geom_bar(data=results.low[nrow(results.low):1,], 
						 aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-100,100))
			names(cols) <- levels(results$variable)
			p = p + scale_fill_manual('Response', breaks=names(cols), values=cols)
		} else {
			p = ggplot(results, aes(y=value, x=Group, group=variable))
			p = p + geom_bar(stat='identity', aes(fill=variable)) + 
				ylim(c(-5,105)) +
				scale_fill_manual('Response', 
							values=cols, 
							breaks=levels(results$variable),
							labels=levels(results$variable))
		}
		p = p + 
		  	geom_text(data=likert$summary, y=ymin, aes(x=Group, 
  					label=paste(round(low), '%', sep=''), group=Item), 
		  			size=text.size, hjust=1) +
			geom_text(data=likert$summary, aes(x=Group, y=100, 
					label=paste(round(high), '%', sep=''), 
					group=Item), size=text.size, hjust=-.2) +
			coord_flip() +	ylab('Percentage') + xlab('') + 
			theme(axis.ticks=element_blank()) + facet_wrap(~ Item, ncol=1)
	} else { #No grouping
		results = melt(likert$results, id.vars='Item')
		if(ordered) {
			order = likert$summary[order(likert$summary$high),'Item']
			results$Item = factor(results$Item, levels=order)
		}
		ymin = 0
		if(centered) {
			ymin = -100
			rows = which(results$variable %in% names(likert$results)[2:((
				(ncol(likert$results) - 1) / 2) + 1)])
			if(likert$nlevels %% 2 != 0) {
				#Odd number of levels, will remove the middle
				#TODO: Perhaps have an option to leave it.
				rows.mid = which(results$variable %in% names(likert$results)[
					( floor(ncol(likert$results) / 2 + 1) )])
				results[rows.mid, 'value'] = 0
			}
			results[rows, 'value'] = -1 * results[rows, 'value']
			results.low = results[rows,]
			results.high = results[-rows,]
			p <- ggplot(results, aes(y=value, x=Item, group=Item)) + 
				geom_bar(data=results.low[nrow(results.low):1,], 
						 aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-100,100))
			names(cols) <- levels(results$variable)
			p =	p + scale_fill_manual('Response', breaks=names(cols), values=cols)
		} else {
			p = ggplot(results, aes(y=value, x=Item, group=Item))
			p = p + geom_bar(stat='identity', aes(fill=variable)) + ylim(c(-5,105))
			p = p + scale_fill_manual('Response', values=cols, 
							  breaks=levels(results$variable), 
							  labels=levels(results$variable))
		}
		p = p + 
			geom_text(data=likert$summary, y=ymin, aes(x=Item, 
				  			label=paste(round(low), '%', sep='')), 
				  			size=text.size, hjust=1) +
			geom_text(data=likert$summary, y=100, aes(x=Item,
				  			label=paste(round(high), '%', sep='')), 
				  			size=text.size, hjust=-.2) +
			coord_flip() + ylab('Percentage') + xlab('') + 
			theme(axis.ticks=element_blank())
	} 
	class(p) <- c('likert.bar.plot', class(p))
	return(p)
}

#' Print method for \code{\link{plot.likert.bar}}. The primary purpose is to 
#' suppress the "Stacking not well defined when ymin != 0" warning printed
#' by \code{ggplot2} for bar plots that have negative bars (i.e. the centered
#' plots).
#' @param p a plot from \code{\link{plot.likert.bar}}.
#' @param ... other parameters passed to ggplot2.
#' @S3method print likert.bar.plot
#' @export
print.likert.bar.plot <- function(p, ...) {
	suppressWarnings(NextMethod(p, ...))
}
