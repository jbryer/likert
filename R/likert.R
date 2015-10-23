#' Analyze Likert type items.
#'
#' This function will provide various statistics about a set of likert
#' items. The resulting object will have the following items:
#' 
#' \itemize{
#'    \item \code{results} - this data frame will contain a column 'Item', 'Group' (if a 
#'          grouping variable was specified, and a column for each level of the
#'          items (e.g. agree, disagree, etc.). The value within each cell corresponds
#'          to the percentage of responses for that level and group.
#'    \item \code{items} - a copy of the original items data frame.
#'    \item \code{grouping} - a copy of the original grouping vector.
#'    \item \code{nlevels} - the number of levels used in the calculations.
#' }
#'
#' @export
#' @param items data frame containing the likert based items. The variables
#'        in the data frame should be factors.
#' @param summary a pre-summarized data frame. The first column must be the
#'        items and the remaining columns are the levels (e.g. strongly disagree,
#'        disagree, etc).
#' @param grouping (optional) should the results be summarized by the given
#'        grouping variable.
#' @param factors a vector with \code{length(factors) == ncol(items)}
#'        defining which factor each column belongs to. The values correspond
#'        to the factor label.
#' @param importance a data frame of the same dimensions as items containing
#'        an importance rating for each item. The order of columns should match
#'        and the names from items will be used.
#' @param nlevels number of possible levels. Only necessary if there are missing levels.
#' @return a likert class with the following elements: results, items, grouping,
#'        nlevels, and summary.
#' @seealso plot.likert
#' @seealso summary.likert
#' @examples
#' data(pisaitems)
#' items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
#' names(items29) <- c("Magazines", "Comic books", "Fiction", 
#'                    "Non-fiction books", "Newspapers")
#' l29 <- likert(items29)
#' summary(l29)
#' plot(l29)
likert <- function(items, summary,
				   grouping=NULL, 
				   factors=NULL,
				   importance,
				   nlevels=length(levels(items[,1]))) {
	if(!missing(summary)) { #Pre-summarized data
		if(!is.null(grouping) & length(grouping) != nrow(summary)) {
			stop('The length of grouping must be equal to the number of rows in summary.')
		}
		if(!missing(importance)) {
			stop('Pre-summarized data with importance is not currently supported')
		}
		if(is.null(grouping)) {
			r <- list(results=summary, 
					  items=NULL, 
					  grouping=grouping, 
					  nlevels=(ncol(summary)-1),
					  levels=names(summary[,2:ncol(summary)]))
		} else {
			r <- list(results=cbind(Group=grouping, summary), 
					  items=NULL, 
					  grouping=grouping, 
					  nlevels=(ncol(summary)-1),
					  levels=names(summary[,2:ncol(summary)]))
		}
		class(r) <- 'likert'
		return(r)
	} else {
		if(class(items) != 'data.frame') {
			stop(paste0('The items parameter must be a data frame. If trying ',
						'to subset a data frame to analyze only one column, try: ',
						'items=mydf[,1, drop=FALSE].'))
		}
		if(!all(sapply(items, function(x) 'factor' %in% class(x)))) {
			warning('items parameter contains non-factors. Will convert to factors')
			for(i in 1:ncol(items)) {
				items[,i] <- factor(items[,i], levels=1:nlevels)
			}
		}
		if(!all(sapply(items, function(x) { length(levels(x)) }) == nlevels)) {
			stop('All items (columns) must have the same number of levels')
		}
		if(!missing(importance)) {
			if(ncol(importance) != ncol(items) | nrow(importance) != nrow(items)) {
				stop('The dimensions of items and importance must be the same')
			}
			if(!all(sapply(importance, function(x) 'factor' %in% class(x)))) {
				warning('importance parameter contains non-factors. Will convert to factors')
				for(i in 1:ncol(importance)) {
					importance[,i] <- factor(importance[,i], levels=1:nlevels)
				}
			}
			if(!all(sapply(importance, function(x) { length(levels(x)) }) == nlevels)) {
				stop('All columns in importance must have the same number of levels')
			}
		}
		
		if(!is.null(grouping) & !is.null(factors)) {
			stop('Grouping by a grouping column and factor is not supported. 
				 Specify either grouping or factors.')
		}
		
		lowrange <- 1 : ceiling(nlevels / 2 - nlevels %% 2)
		highrange <- ceiling(nlevels / 2 + 1 ) : nlevels
		
		results <- data.frame()
		if(!is.null(grouping)) {
			if(is.numeric(grouping)) {
				grouping <- as.character(grouping)
			}
			results <- data.frame(
				Group = rep(unique(grouping), each=nlevels),
				Response = rep(1:nlevels, length(unique(grouping)))
				)
			for(i in 1:ncol(items)) {
				t <- as.data.frame(table(grouping, as.integer(items[,i])))
				t <- reshape2::dcast(t, Var2 ~ grouping, value.var='Freq', add.missing=TRUE)
				t <- cbind(Response=t[,1], 
						   apply(t[,2:ncol(t)], 2, FUN=function(x) { x / sum(x) * 100 } )
				)
				t <- reshape2::melt(t)
				results <- merge(results, t, 
								 by.x=c('Group','Response'), by.y=c('Var2','Var1'), 
								 all.x=TRUE)
				names(results)[ncol(results)] <- paste0('Col', i)
			}
			
			names(results)[3:ncol(results)] <- names(items)
			
			results$Response <- factor(results$Response, levels=1:nlevels, 
									  labels=levels(items[,i]))
			results <- reshape2::melt(results, id=c('Group', 'Response'))
			results <- reshape2::dcast(results, Group + variable ~ Response)
			results <- as.data.frame(results)
			names(results)[2] <- 'Item'
			
			for(i in 3:ncol(results)) {
				narows <- which(is.na(results[,i]))
				if(length(narows) > 0) {
					results[narows, i] <- 0
				}
			}
		} else {
			results <- data.frame(Response=1:nlevels)
			means <- numeric()
			sds <- numeric()
			for(i in 1:ncol(items)) {
				t <- table(items[,i])
				t <- (t / sum(t) * 100)
				means[i] <- mean(as.numeric(items[,i]), na.rm=TRUE)
				sds[i] <- sd(as.numeric(items[,i]), na.rm=TRUE)
				results <- cbind(results, as.data.frame(t)[,2])
				#results <- merge(results, as.data.frame(t),
				#				 by.x='Response', by.y='Var1', all.x=TRUE)
				names(results)[ncol(results)] <- names(items)[i]
			}
			results <- as.data.frame(t(results))
			names(results) <- levels(items[,1])
			results <- results[2:nrow(results),]
			
			results <- cbind(row.names(results), results)
			names(results)[1] <- 'Item'
			row.names(results) <- 1:nrow(results) 
	
			for(i in 2:ncol(results)) {
				narows <- which(is.na(results[,i]))
				if(length(narows) > 0) {
					results[narows, i] <- 0
				}
			}
			
			if(!is.null(factors)) {
				if(length(factors) != nrow(results)) {
					stop('length(factors) != ncol(items)')
				}
				results <- cbind(results[,1,drop=FALSE], 
								 'Group'=factors, 
								 results[,2:ncol(results)])
			}
		}
		
		r <- list(results=results, items=items, grouping=grouping, factors=factors,
				  nlevels=nlevels, levels=levels(items[,1]))
		if(!missing(importance)) {
			names(importance) <- names(items)
			r$importance <- likert(importance, grouping=grouping, nlevels=nlevels)
			class(r) <- c('likert.gap', 'likert')
		} else {
			class(r) <- 'likert'
		}
		return(r)
	}
}
