utils::globalVariables(c('group'))

#' Histogram of number of responses.
#' 
#' Plots a histogram of the number of responses for each item and group (if specified).
#' Negative values (in maroon by default) indicate the number of missing values
#' for that item and group.
#' 
#' @param l results of \code{\link{likert}}.
#' @param xlab label used for the x-axis.
#' @param plot.missing if TRUE, missing values will be plotted to the left of the
#'        x-axis.
#' @param bar.color the bar color.
#' @param missing.bar.color the color of the bar for missing values.
#' @param label.completed the label to use in the legend representing the count of
#'        complete values.
#' @param label.missing the label to use in the legend representing the count of
#'        missing values.
#' @param order the order of the items.
#' @param ... other ggplot2 parameters.
#' @inheritParams likert.bar.plot
#' @export
likert.histogram.plot <- function(l, 
								  xlab='n',
								  plot.missing=TRUE,
								  bar.color='grey70',
								  missing.bar.color='maroon',
								  label.completed='Completed',
								  label.missing='Missing',
								  legend.position='bottom',
								  wrap=ifelse(is.null(l$grouping), 50, 100),
								  order,
								  group.order,
								  panel.arrange='v',
								  panel.strip.color='#F0F0F0',
								  ...) {
	nacount <- function(items) {
		hist <- as.data.frame(sapply(items, function(x) { table(is.na(x)) }), 
							  stringsAsFactors=FALSE)
		hist$missing <- row.names(hist)
		hist <- melt(hist, id.vars='missing', variable_name='Item')
		hist$missing <- as.logical(hist$missing)
		hist[hist$missing,]$value <- -1 * hist[hist$missing,]$value
		return(hist)
	}

	if(missing(order)) {
		order <- names(items)
	}
	
	items <- l$items
	if(is.null(l$grouping)) {
		hist <- nacount(items)
		hist$Item <- label_wrap_mod(hist$Item, width=wrap)
		hist$Item <- factor(hist$Item, 
							levels=label_wrap_mod(order, width=wrap), 
							ordered=TRUE)

		p <- ggplot(hist, aes(x=Item, y=value, fill=missing))
		if(plot.missing) {
			p <- p + geom_bar(data=hist[hist$missing,], stat='identity')
		}
		p <- p +
			geom_bar(data=hist[!hist$missing,], stat='identity') +
			geom_hline(yintercept=0) +
			scale_y_continuous(label=abs_formatter) +
			coord_flip() + ylab(xlab) + xlab('') +
			theme(legend.position=legend.position) +
			scale_fill_manual('',
							  limits=c(TRUE,FALSE),
							  labels=c(label.missing, label.completed),
							  values=c(missing.bar.color, bar.color))
	} else {
		hist <- data.frame()
		for(g in unique(l$grouping)) {
			h <- nacount(items[l$grouping == g,])
			h$group <- g
			hist <- rbind(hist, h)
		}
		
		hist$Item <- label_wrap_mod(hist$Item, width=wrap)
		hist$Item <- factor(hist$Item, 
							levels=label_wrap_mod(order, width=wrap), 
							ordered=TRUE)
		
		p <- ggplot(hist, aes(x=group, y=value, fill=missing))
		if(plot.missing) {
			p <- p + geom_bar(data=hist[hist$missing,], stat='identity')
		}
		p <- p +
			geom_bar(data=hist[!hist$missing,], stat='identity') +
			geom_hline(yintercept=0) +
			scale_y_continuous(label=abs_formatter) +
			coord_flip() + ylab(xlab) + xlab('') +
			scale_fill_manual('',
							  limits=c(TRUE,FALSE),
							  labels=c(label.missing, label.completed),
							  values=c(missing.bar.color, bar.color)) +
			theme(legend.position=legend.position,
				  axis.ticks=element_blank(), 
				  strip.background=element_rect(fill=panel.strip.color, 
				  							    color=panel.strip.color))
		if(is.null(panel.arrange)) {
			p <- p + facet_wrap(~ Item)
		} else if(panel.arrange == 'v') {
			p <- p + facet_wrap(~ Item, ncol=1)
			#p <- p + facet_grid(Item ~ .)
		} else if(panel.arrange == 'h') {
			p <- p + facet_wrap(~ Item, nrow=1)
		}
		if(!missing(group.order)) {
			p <- p + scale_x_discrete(limits=rev(group.order))
		}
	}
	class(p) <- c('likert.bar.plot', class(p))
	return(p)
}
