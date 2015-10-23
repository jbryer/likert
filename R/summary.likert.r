#' Prints summary table of a Likert analysis.
#'
#' The \code{summary} function returns a data frame that provides additional 
#' information. It contains 'Item' and 'Group' columns similiar to the results data
#' frame as well as a column 'low' corresponding to the sum of levels below
#' neutral, a column 'high' corresponding to the sum of levels above
#' neutral, and columns 'mean' and 'sd' corresponding to the mean and
#' standard deviation, respectively, of the results. The numeric values
#' are determined by as.numeric which will use the values of the factors.
#'          
#' @param object the likert class to summarize.
#' @param center specifies which level should be treated as the center. For example,
#'        \code{center = 3} would use the third level as the center whereas
#'        \code{center = 3.5} would indicate no specific level is the center but
#'        <= 3 are low levels and >= 4 are high levels (i.e. used for forced choice 
#'        items or those without a neutral option).
#' @param ordered whether the results should be ordered. Currently unsupported
#'        for grouped analysis.
#' @param ... currently unused.
#' @export
#' @method summary likert
summary.likert <- function(object, center=(object$nlevels-1)/2 + 1,
						   ordered=TRUE, ...) {
	if(center < 1.5 | center > (object$nlevels - 0.5) | center %% 0.5 != 0) {
		stop(paste0('Invalid center. Values can range from 1.5 to ', 
					(object$nlevels - 0.5), ' in increments of 0.5'))
	}
	
	if(is.null(object$items)) { # Pre-summarized data
		results <- object$results
		startCol <- ifelse(is.null(object$grouping), 2, 3)
		if(any(apply(results[,startCol:ncol(results)], 1, FUN=sum) < 1.05)) { 
			# Something a little bigger than 1 to account for rounding errors. 
			# If TRUE, then the percentages range from 0 to 1.
			results[,startCol:ncol(results)] <- 100 * results[,startCol:ncol(results)]
		} #TODO: Could add a check to ensure the sum of all rows equal 1 or 100
		tmp <- t(apply(results[,startCol:ncol(results)], 1, FUN=function(x) {
			tmp <- rep(1:length(x), x)
			return(c(mean=mean(tmp), sd=sd(tmp)))
		}))
		if(center %% 1 == 0) {
			neutral <- results[,(center+(startCol-1))]
		} else {
			netural <- NA
		}
		if(is.null(object$grouping)) {
			lowCols <- startCol:center
			if(length(lowCols) == 1) {
				low <- results[,lowCols]
			} else {
				low <- apply(results[,lowCols], 1, sum)
			}
			highCols <- (center+startCol):ncol(results)
			if(length(highCols) == 1) {
				high <- results[,highCols]
			} else {
				high <- apply(results[,highCols], 1, sum)
			}
			results2 <- data.frame(Item=results[,1],
								   low=low,
								   neutral=neutral,
								   high=high,
								   mean=tmp[,1],
								   sd=tmp[,2]  )
		} else {
			lowCols <- startCol:(center+1)
			if(length(lowCols) == 1) {
				low <- results[,lowCols]
			} else {
				low <- apply(results[,lowCols], 1, sum)
			}
			highCols <- (center+startCol):ncol(results)
			if(length(highCols) == 1) {
				high <- results[,highCols]
			} else {
				high <- apply(results[,highCols], 1, sum)
			}
			results2 <- data.frame(Group=results[,1],
								   Item=results[,2],
								   low=low,
								   neutral=neutral,
								   high=high,
								   mean=tmp[,1],
								   sd=tmp[,2]  )			
		}
		return(results2)
	} else {
		results <- object$results
		items <- object$items
		nlevels <- object$nlevels
		lowrange <- 1 : floor(center - 0.5)
		highrange <- ceiling(center + 0.5) : nlevels
		if(!is.null(object$grouping)) { #Grouping
			results2 = data.frame(Group=rep(unique(results$Group), each=ncol(items)),
								  Item=rep(names(items), length(unique(results$Group))), 
								  low=rep(NA, ncol(items) * length(unique(results$Group))), 
								  neutral=rep(NA, ncol(items) * length(unique(results$Group))),
								  high=rep(NA, ncol(items) * length(unique(results$Group))),
								  mean=rep(NA, ncol(items) * length(unique(results$Group))),
								  sd=rep(NA, ncol(items) * length(unique(results$Group))) )
			for(g in unique(results$Group)) {
				if(length(lowrange) == 1) {
					results2[which(results2$Group == g),]$low <-
						results[results$Group == g, lowrange + 2]
				} else {
					results2[which(results2$Group == g),]$low = apply(
						results[results$Group == g, lowrange + 2], 1, sum)
				}
				if(length(highrange) == 1) {
					results2[which(results2$Group == g),]$high <-
						results[results$Group == g, highrange + 2]
				} else {
					results2[which(results2$Group == g),]$high = apply(
						results[results$Group == g,highrange + 2], 1, sum)
				}
				if(lowrange[length(lowrange)] + 1 != highrange[1]) {
					results2[which(results2$Group == g),]$neutral <- 
						results[results$Group == g, (highrange[1] - 1 + 2)]
				}
				for(i in names(items)) {
					results2[which(results2$Group == g & results2$Item == i), 'mean'] = 
						mean(as.numeric(items[which(object$grouping == g), i]), na.rm=TRUE)
					results2[which(results2$Group == g & results2$Item == i), 'sd'] = 
						sd(as.numeric(items[which(object$grouping == g), i]), na.rm=TRUE)
				}
			}
		} else { #No grouping
			results = data.frame(Response=1:nlevels)
			means = numeric()
			sds = numeric()
			for(i in 1:ncol(items)) {
				t = table(items[,i])
				t = (t / sum(t) * 100)
				means[i] = mean(as.numeric(items[,i]), na.rm=TRUE)
				sds[i] = sd(as.numeric(items[,i]), na.rm=TRUE)
				results = cbind(results, as.data.frame(t)[2])
				names(results)[ncol(results)] = names(items)[i]
			}
			results = as.data.frame(t(results))
			names(results) = levels(items[,1])
			results = results[2:nrow(results),]
			results2 = data.frame(Item=row.names(results),
								  low=rep(NA, nrow(results)),
								  neutral=rep(NA, nrow(results)),
								  high=rep(NA, nrow(results)),
								  mean=means, 
								  sd=sds)
			if(length(lowrange) == 1) {
				results2$low <- results[,lowrange]
			} else {
				results2$low <- apply(results[,lowrange], 1, sum)
			}
			if(length(highrange) == 1) {
				results2$high <- results[,highrange]
			} else {
				results2$high <- apply(results[,highrange], 1, sum)
			}
			if(lowrange[length(lowrange)] + 1 != highrange[1]) {
				results2$neutral <- results[,(highrange[1] - 1)]
			}
			row.names(results2) = 1:nrow(results2)
			if(!is.null(object$factors)) {
				results2 <- cbind(results2[,1,drop=FALSE],
								  'Factor'=object$factors,
								  results2[,2:ncol(results2),drop=FALSE])
			}
			if(ordered) {
				results2 <- results2[order(results2$high, decreasing=TRUE),]
			}
		}
		
		narows <- which(is.na(results2$low))
		if(length(narows) > 0) {
			results2[narows,]$low <- 0
		}
		narows <- which(is.na(results2$neutral))
		if(length(narows) > 0) {
			results2[narows,]$neutral <- 0
		}
		narows <- which(is.na(results2$high))
		if(length(narows) > 0) {
			results2[narows,]$high <- 0
		}
		
		return(results2)
	}
}
