#' Bar Plot for Likert Items.
#' 
#' Bar plot for the results of \code{\link{likert}}.
#' 
#' @param likert object of type likert.
#' @param low.color color for low values.
#' @param high.color color for high values.
#' @param neutral.color color for middle values (if odd number of levels).
#' @param neutral.color.ramp second color used when calling \code{\link{colorRamp}}
#'        with \code{low.color} and \code{high.color} to define the color palettes.
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
#' @param wrap width to wrap label text for non-grouped likert objects.
#' @param ... currently unused.
#' @export
#' @seealso plot.likert
#' @seealso likert.heat.plot
likert.bar.plot <- function(likert,
							low.color='#D8B365',
							high.color='#5AB4AC', 
							neutral.color='grey90',
							neutral.color.ramp='white',
							text.size=3,
							text.color='black',
							centered=FALSE,
							center=(likert$nlevels-1)/2 + 1,
							include.center=TRUE,
							ordered=TRUE,
							wrap=ifelse(is.null(likert$grouping), 50, 100),
							...) {
	if(center < 1.5 | center > (likert$nlevels - 0.5) | center %% 0.5 != 0) {
		stop(paste0('Invalid center. Values can range from 1.5 to ', 
					(likert$nlevels - 0.5), ' in increments of 0.5'))
	}
	
	lowrange <- 1 : floor(center - 0.5)
	highrange <- ceiling(center + 0.5) : likert$nlevels
	ramp <- colorRamp(c(low.color, neutral.color.ramp))
	ramp <- rgb( ramp(seq(0, 1, length=length(lowrange)+1)), max=255)
	bamp <- colorRamp(c(neutral.color.ramp, high.color))
	bamp <- rgb( bamp(seq(0, 1, length=length(highrange)+1)), max=255)
	cols <- NULL
	if(center %% 1 != 0) {
		cols <- c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])		
	} else {
		cols <- c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
	}
	
	lsum <- summary(likert, center=center)
	
	p <- NULL
	if(!is.null(likert$grouping)) {
		lsum$Item <- likert:::label_wrap_mod(lsum$Item, width=wrap)
		likert$results$Item <- likert:::label_wrap_mod(likert$results$Item, width=wrap)
		names(likert$items) <- likert:::label_wrap_mod(names(likert$items), width=wrap)
		
		results <- melt(likert$results, id=c('Group', 'Item'))
		results$variable <- factor(results$variable, ordered=TRUE)
		ymin <- 0

		if(centered) {
			ymin <- -100
			rows <- which(results$variable %in% names(likert$results)[
				3:(length(lowrange)+2)])
			results[rows,'value'] <- -1 * results[rows,'value']
			if(center %% 1 == 0) { #Midpoint is a level
				rows.mid <- which(results$variable %in% names(likert$results)[center+2])
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
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-105,105))
			names(cols) <- levels(results$variable)
			p <- p + scale_fill_manual('Response', breaks=names(cols), values=cols)
		} else {
			p <- ggplot(results, aes(y=value, x=Group, group=variable))
			p <- p + geom_bar(stat='identity', aes(fill=variable)) + 
				ylim(c(-5,105)) +
				scale_fill_manual('Response', 
							values=cols, 
							breaks=levels(results$variable),
							labels=levels(results$variable))
		}
		p <- p + 
		  	geom_text(data=lsum, y=ymin, aes(x=Group, 
  					label=paste0(round(low), '%'), group=Item), 
		  			size=text.size, hjust=1) +
			geom_text(data=lsum, aes(x=Group, y=100, 
					label=paste0(round(high), '%'), 
					group=Item), size=text.size, hjust=-.2)
		if(!any(is.na(lsum$neutral)) & include.center) {
			p <- p +
				geom_text(data=lsum, y=0, aes(x=Group,
					label=paste0(round(neutral), '%'), group=Item),
					size=text.size, hjust=.5)
		}
		p <- p +
			coord_flip() +	ylab('Percentage') + xlab('') + 
			theme(axis.ticks=element_blank()) + facet_wrap(~ Item, ncol=1)
	} else { #No grouping
		results <- melt(likert$results, id.vars='Item')
		if(ordered) {
			order <- lsum[order(lsum$high),'Item']
			results$Item <- factor(results$Item, levels=order)
		}
		ymin <- 0
		if(centered) {
			ymin <- -100
			rows <- which(results$variable %in% names(likert$results)[
				2:(length(lowrange) + 1)])
			results[rows, 'value'] <- -1 * results[rows, 'value']
			if(center %% 1 == 0) { # Midpoint is a level (i.e. there are an odd number of levelss)
				rows.mid <- which(results$variable %in% names(likert$results)[center+1])
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
			results.low <- results[results$value < 0,]
			results.high <- results[results$value > 0,]
			p <- ggplot(results, aes(y=value, x=Item, group=Item)) + 
				geom_hline(yintercept=0) +
				geom_bar(data=results.low[nrow(results.low):1,], 
						 aes(fill=variable), stat='identity') + 
				geom_bar(data=results.high, aes(fill=variable), stat='identity') +
				ylim(c(-105,105))
			names(cols) <- levels(results$variable)
			p <- p + scale_fill_manual('Response', breaks=names(cols), values=cols)
		} else {
			p <- ggplot(results, aes(y=value, x=Item, group=Item))
			p <- p + geom_bar(stat='identity', aes(fill=variable)) + ylim(c(-5,105))
			p <- p + scale_fill_manual('Response', values=cols, 
							  breaks=levels(results$variable), 
							  labels=levels(results$variable))
		}
		p <- p + 
			geom_text(data=lsum, y=ymin, aes(x=Item, 
				  			label=paste0(round(low), '%')), 
				  			size=text.size, hjust=1) +
			geom_text(data=lsum, y=100, aes(x=Item,
				  			label=paste0(round(high), '%')), 
				  			size=text.size, hjust=-.2)
		if(!any(is.na(lsum$neutral)) & include.center) {
			if(centered) {
				p <- p +
					geom_text(data=lsum, y=0, 
							  aes(x=Item, label=paste0(round(neutral), '%')),
							  size=text.size, hjust=.5)
			} else {
				lsum$y <- lsum$low + (lsum$neutral/2)
				p <- p +
					geom_text(data=lsum,
							  aes(x=Item, y=y, label=paste0(round(neutral), '%')),
							  size=text.size, hjust=.5)				
			}
		}
		p <- p +
			coord_flip() + ylab('Percentage') + xlab('') + 
			theme(axis.ticks=element_blank()) +
			scale_x_discrete(labels=likert:::label_wrap_mod(likert$results$Item, width=wrap))
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
