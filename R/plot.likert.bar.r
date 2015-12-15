utils::globalVariables(c('value','Group','variable','low','Item','high',
						 'neutral','x','y','pos','ddply','.'))

#' Bar Plot for Likert Items.
#' 
#' Bar plot for the results of \code{\link{likert}}.
#' 
#' @param l results of \code{\link{likert}}.
#' @param likert object of type likert.
#' @param low.color color for low values.
#' @param high.color color for high values.
#' @param neutral.color color for middle values (if odd number of levels).
#' @param neutral.color.ramp second color used when calling \code{\link{colorRamp}}
#'        with \code{low.color} and \code{high.color} to define the color palettes.
#' @param colors vector specifying the colors to use. This must be equal to 
#'        the number of likert levels.
#' @param plot.percent.low whether to plot low percentages.
#' @param plot.percent.high whether to plot high percentages.
#' @param plot.percent.neutral whether to plot netural percentages.
#' @param plot.percents whether to label each category/bar.
#' @param text.size size of text attributes.
#' @param text.color color of text attributes.
#' @param centered if true, the bar plot will be centered around zero such that
#'        the lower half of levels will be negative.
#' @param center specifies which level should be treated as the center. For example,
#'        \code{center = 3} would use the third level as the center whereas
#'        \code{center = 3.5} would indicate no specific level is the center but
#'        <= 3 are low levels and >= 4 are high levels (i.e. used for forced choice 
#'        items or those without a neutral option). This also influences where the
#'        color breaks from low to high.
#' @param include.center if TRUE, include the center level in the plot otherwise
#'        the center will be excluded.
#' @param ordered reorder items from high to low.
#' @param wrap width to wrap label text for item labels
#' @param wrap.grouping width to wrap label text for group labels.
#' @param legend title for the legend.
#' @param legend.position the position for the legend ("left", "right", "bottom",
#'        "top", or two-element numeric vector).
#' @param panel.arrange how panels for grouped likert items should be arrange.
#'        Possible values are \code{v} (vertical, the default), \code{h}
#'        (horizontal), and \code{NULL} (auto fill horizontal and vertical)
#' @param panel.strip.color the background color for panel labels.
#' @param group.order the order in which groups (for grouped items) or items
#'        (for non-grouped items) should be plotted.
#' @param ... currently unused.
#' @export
#' @seealso plot.likert
#' @seealso likert.heat.plot
#' @seealso likert.bar.plot
#' @seealso likert.density.plot
likert.bar.plot <- function(l,
							low.color='#D8B365',
							high.color='#5AB4AC',
							neutral.color='grey90',
							neutral.color.ramp='white',
							colors=NULL,
							plot.percent.low=TRUE,
							plot.percent.high=TRUE,
							plot.percent.neutral=TRUE,
							plot.percents=FALSE,
							text.size=3,
							text.color='black',
							centered=TRUE,
							center=(l$nlevels-1)/2 + 1,
							include.center=TRUE,
							ordered=TRUE,
							wrap=ifelse(is.null(l$grouping), 50, 100),
							wrap.grouping=50,
							legend='Response',
							legend.position='bottom',
							panel.arrange='v',
							panel.strip.color='#F0F0F0',
							group.order,
							...) {
	if(center < 1.5 | center > (l$nlevels - 0.5) | center %% 0.5 != 0) {
		stop(paste0('Invalid center. Values can range from 1.5 to ', 
					(l$nlevels - 0.5), ' in increments of 0.5'))
	}
	ymin <- 0
	ymax <- 100
	ybuffer <- 5
	
	lowrange <- 1 : floor(center - 0.5)
	highrange <- ceiling(center + 0.5) : l$nlevels
	cols <- NULL
	if(!is.null(colors) & length(colors) == l$nlevels) {
		cols <- colors
	} else {
		if(!is.null(colors) & length(colors) != l$nlevels) {
			warning('The length of colors must be equal the number of levels.')
		}
		ramp <- colorRamp(c(low.color, neutral.color.ramp))
		ramp <- rgb( ramp(seq(0, 1, length=length(lowrange)+1)), maxColorValue=255)
		bamp <- colorRamp(c(neutral.color.ramp, high.color))
		bamp <- rgb( bamp(seq(0, 1, length=length(highrange)+1)), maxColorValue=255)
		cols <- NULL
		if(center %% 1 != 0) {
			cols <- c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])		
		} else {
			cols <- c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
		}		
	}

	lsum <- summary(l, center=center)
	
	p <- NULL
	if(!is.null(l$grouping)) { ##### Grouping ##################################
		lsum$Item <- label_wrap_mod(lsum$Item, width=wrap)
		l$results$Item <- label_wrap_mod(l$results$Item, width=wrap)
		#names(l$items) <- label_wrap_mod(names(l$items), width=wrap)
		lsum$Group <- label_wrap_mod(lsum$Group, width=wrap.grouping)
		
		results <- l$results
		results <- reshape2::melt(results, id=c('Group', 'Item'))
		results$variable <- factor(results$variable, ordered=TRUE)
		if(TRUE | is.null(l$items)) {
			results$Item <- factor(as.character(results$Item),
								   levels=unique(results$Item),
								   labels=label_wrap_mod(
								   	as.character(unique(results$Item)), width=wrap),
								   ordered=TRUE)
		} else {
			results$Item <- factor(results$Item,
								   levels=label_wrap_mod(names(l$items), width=wrap),
								   ordered=TRUE)
		}
		ymin <- 0

		if(centered) {
			ymin <- -100
			rows <- which(results$variable %in% names(l$results)[
				3:(length(lowrange)+2)])
			results[rows,'value'] <- -1 * results[rows,'value']
			if(center %% 1 == 0) { #Midpoint is a level
				rows.mid <- which(results$variable %in% names(l$results)[center+2])
				if(include.center) {
					tmp <- results[rows.mid,]
					tmp$value <- tmp$value / 2 * -1
					results[rows.mid,'value'] <- results[rows.mid,'value'] / 2
					results <- rbind(results, tmp)
				} else {
					results <- results[-rows.mid,]
				}
			}
			results.low <- results[results$value < 0,]
			results.high <- results[results$value > 0,]
			p <- ggplot(results, aes(y=value, x=Group, group=variable)) + 
				geom_hline(yintercept=0) +
				geom_bar(data=results.low[nrow(results.low):1,], 
						 aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity')
			names(cols) <- levels(results$variable)
			p <- p + scale_fill_manual(legend, breaks=names(cols), values=cols, drop=FALSE)
		} else {
			ymin <- 0
			p <- ggplot(results, aes(y=value, x=Group, group=variable))
			p <- p + geom_bar(stat='identity', aes(fill=variable)) +
				scale_fill_manual(legend, 
							values=cols, 
							breaks=levels(results$variable),
							labels=levels(results$variable),
							drop=FALSE)
		}
		if(plot.percent.low) {
			p <- p + geom_text(data=lsum, y=ymin, aes(x=Group, 
	  						   label=paste0(round(low), '%'), group=Item), 
			  				   size=text.size, hjust=1, color=text.color)
		}
		if(plot.percent.high) {
			p <- p + geom_text(data=lsum, aes(x=Group, y=100, 
							   label=paste0(round(high), '%'), 
							   group=Item), size=text.size, hjust=-.2, color=text.color)			
		}
		if(plot.percent.neutral & l$nlevels %% 2 == 1 & include.center) {
			if(centered) {
				p <- p + geom_text(data=lsum, y=0, aes(x=Group, group=Item,
							  	   label=paste0(round(neutral), '%')),
							       size=text.size, hjust=.5, color=text.color)
			} else {
				lsum$y <- lsum$low + (lsum$neutral/2)
				p <- p + geom_text(data=lsum, aes(x=Group, y=y, group=Item,
							  	   label=paste0(round(neutral), '%')),
							       size=text.size, hjust=.5, color=text.color)				
			}
		}
		if(FALSE & plot.percents) { #TODO: implement for grouping
			warning('plot.percents is not currenlty supported for grouped analysis.')
# 			lpercentpos <- ddply(results[results$value > 0,], .(Item), transform, 
# 								 pos = cumsum(value) - 0.5*value)
# 			p + geom_text(data=lpercentpos, aes(x=Group, y=pos, label=paste0(round(value), '%'),
# 												group=Item),
# 							   size=text.size)
# 			lpercentneg <- results[results$value < 0,]
# 			if(nrow(lpercentneg) > 0) {
# 				lpercentneg <- lpercentneg[nrow(lpercentneg):1,]
# 				lpercentneg$value <- abs(lpercentneg$value)
# 				lpercentneg <- ddply(lpercentneg, .(Item), transform, 
# 									 pos = cumsum(value) - 0.5*value)	
# 				lpercentneg$pos <- lpercentneg$pos * -1
# 				p <- p + geom_text(data=lpercentneg, aes(x=Item, y=pos, label=paste0(round(abs(value)), '%')),
# 								   size=text.size)
# 			}
		}
		p <- p +
			coord_flip() +	ylab('Percentage') + xlab('') +
			theme(axis.ticks=element_blank(), 
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
			p <- p + scale_x_discrete(limits=rev(group.order), drop=FALSE)
		}
	} else { ##### No grouping #################################################
		factor.mapping <- NULL
		if(!is.null(l$factors)) {
			factor.mapping <- l$results[,1:2]
			names(factor.mapping)[2] <- 'Factor'
			results <- reshape2::melt(l$results[,-2], id.vars='Item')
		} else {
			results <- reshape2::melt(l$results, id.vars='Item')
		}
		if(ordered & is.null(results$factor)) {
			order <- lsum[order(lsum$high),'Item']
			results$Item <- factor(results$Item, levels=order)
		}
		ymin <- 0
		if(centered) {
			ymin <- -100
			rows <- which(results$variable %in% names(l$results)[
				2:(length(lowrange) + 1)])
			results[rows, 'value'] <- -1 * results[rows, 'value']
			if(center %% 1 == 0) { # Midpoint is a level (i.e. there are an odd number of levels)
				rows.mid <- which(results$variable %in% names(l$results)[center+1])
				if(include.center) {
					tmp <- results[rows.mid,]
					tmp$value <- tmp$value/2 * -1
					results[rows.mid,'value'] <- results[rows.mid,'value'] / 2
					results <- rbind(results, tmp)
				} else {
					#results[rows.mid,'value'] <- 0
					results <- results[-rows.mid,]
				}
			}
			if(!is.null(factor.mapping)) {
				results$order <- 1:nrow(results)
				results <- merge(results, factor.mapping,
								 by='Item', all.x=TRUE)
				results <- results[order(results$order),]
				results$order <- NULL
			}
			results.low <- results[results$value < 0,]
			results.high <- results[results$value > 0,]
			p <- ggplot(results, aes(y=value, x=Item, group=Item)) + 
				geom_hline(yintercept=0) +
				geom_bar(data=results.low[nrow(results.low):1,], 
						 aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity')
			names(cols) <- levels(results$variable)
			p <- p + scale_fill_manual(legend, breaks=names(cols), values=cols, drop=FALSE)
		} else {
			if(!is.null(factor.mapping)) {
				results$order <- 1:nrow(results)
				results <- merge(results, factor.mapping,
								 by='Item', all.x=TRUE)
				results <- results[order(results$order),]
				results$order <- NULL
			}
			p <- ggplot(results, aes(y=value, x=Item, group=Item))
			p <- p + geom_bar(stat='identity', aes(fill=variable))
			p <- p + scale_fill_manual(legend, values=cols, 
							  breaks=levels(results$variable), 
							  labels=levels(results$variable),
							  drop=FALSE)
		}
		if(plot.percent.low) {
			p <- p + geom_text(data=lsum, y=ymin, aes(x=Item, 
					  			label=paste0(round(low), '%')), 
					  			size=text.size, hjust=1, color=text.color)
		}
		if(plot.percent.high) {
			p <- p + geom_text(data=lsum, y=100, aes(x=Item,
				  			label=paste0(round(high), '%')), 
				  			size=text.size, hjust=-.2, color=text.color)
		}
		if(plot.percent.neutral & l$nlevels %% 2 == 1 & include.center) {
			if(centered) {
				p <- p +
					geom_text(data=lsum, y=0, 
							  aes(x=Item, label=paste0(round(neutral), '%')),
							  size=text.size, hjust=.5, color=text.color)
			} else {
				lsum$y <- lsum$low + (lsum$neutral/2)
				p <- p +
					geom_text(data=lsum,
							  aes(x=Item, y=y, label=paste0(round(neutral), '%')),
							  size=text.size, hjust=.5, color=text.color)				
			}
		}
		if(plot.percents) {
			lpercentpos <- ddply(results[results$value > 0,], .(Item), transform, 
								 pos = cumsum(value) - 0.5*value)
			p <- p + geom_text(data=lpercentpos, aes(x=Item, y=pos, 
						label=paste0(round(value), '%')),
						size=text.size, color=text.color)
			lpercentneg <- results[results$value < 0,]
			if(nrow(lpercentneg) > 0) {
				lpercentneg <- lpercentneg[nrow(lpercentneg):1,]
				lpercentneg$value <- abs(lpercentneg$value)
				lpercentneg <- ddply(lpercentneg, .(Item), transform, 
									 pos = cumsum(value) - 0.5*value)	
				lpercentneg$pos <- lpercentneg$pos * -1
				p <- p + geom_text(data=lpercentneg, aes(x=Item, y=pos, 
							label=paste0(round(abs(value)), '%')),
							size=text.size, color=text.color)
			}
		}
		p <- p +
			coord_flip() + ylab('Percentage') + xlab('') + 
			theme(axis.ticks=element_blank())
		if(!is.null(factor.mapping)) {
			# DOES NOT WORK! Not supported
			# p + facet_wrap(~ Factor, ncol=1, scales='free')
		}
		if(!missing(group.order)) {
			p <- p + scale_x_discrete(limits=rev(group.order),
				labels=label_wrap_mod(rev(group.order), width=wrap), drop=FALSE)
		} else {
			p <- p + scale_x_discrete(breaks=l$results$Item,
				labels=label_wrap_mod(l$results$Item, width=wrap), drop=FALSE)
		}
	}
	p <- p + scale_y_continuous(labels=abs_formatter, 
								limits=c(ymin - ybuffer, ymax + ybuffer))
	p <- p + theme(legend.position=legend.position)
	
	attr(p, 'item.order') <- levels(results$Item)
	class(p) <- c('likert.bar.plot', class(p))
	return(p)
}

#' Print method for \code{\link{likert.bar.plot}}. The primary purpose is to 
#' suppress the "Stacking not well defined when ymin != 0" warning printed
#' by \code{ggplot2} for bar plots that have negative bars (i.e. the centered
#' plots).
#' @param x a plot from \code{\link{likert.bar.plot}}.
#' @param ... other parameters passed to ggplot2.
#' @export
#' @method print likert.bar.plot
print.likert.bar.plot <- function(x, ...) {
	suppressWarnings(NextMethod(x, ...))
}
