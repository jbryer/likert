#' Analyze Likert type items.
#'
#' This function will provide various summary statistics about a set of likert
#' items. The resulting object will have the following items:
#' 
#' \itemize{
#'    \item results this data frame will contain a column 'Item', 'Group' (if a 
#'          grouping variable was specified, and a column for each level of the
#'          items (e.g. agree, disagree, etc.). The value within each cell corresponds
#'          to the percentage of responses for that level and group.
#'    \item items a copy of the original items data frame.
#'    \item grouping a copy of the original grouping vector.
#'    \item nlevels the number of levels used in the calculations.
#' }
#'
#' @export
#' @param items data frame containing the likert based items. The variables
#'        in the data frame should be factors.
#' @param grouping (optional) should the results be summarized by the given
#'        grouping variable.
#' @param nlevels number of possible levels. Only necessary if there are missing levels.
#' @return a likert class with the following elements: results, items, grouping,
#'        nlevels, and summary.
#' @seealso \code{\link{plot.likert}}, \code{\link{summary.likert}}
#' @examples
#' data(pisaitems)
#' items29 = pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
#' names(items29) = c("Magazines", "Comic books", "Fiction", 
#'                    "Non-fiction books", "Newspapers")
#' l29 = likert(items29)
#' summary(l29)
#' plot(l29)
likert <- function(items, grouping=NULL, nlevels=length(levels(items[,1]))) {
	lowrange = 1 : ceiling(nlevels / 2 - nlevels %% 2)
	highrange = ceiling(nlevels / 2 + 1 ) : nlevels
	
	results <- data.frame()
	if(!is.null(grouping)) {
		results = data.frame(
			Group = rep(unique(grouping), each=nlevels),
			Response = rep(1:nlevels, length(unique(grouping)))
			)
		for(i in 1:ncol(items)) {
			t = as.data.frame(table(grouping, items[,i]))
			t = cast(t, Var2 ~ grouping, value='Freq', add.missing=TRUE)
			t = apply(t, 2, FUN=function(x) { x / sum(x) * 100 } )
			t = melt(t)
			results = cbind(results, t[,3])
		}
		
		names(results)[3:ncol(results)] = names(items)
		
		results$Response = factor(results$Response, levels=1:nlevels, 
								  labels=levels(items[,i]))
		results = melt(results, id=c('Group', 'Response'))
		results = cast(results, Group + variable ~ Response)
		results = as.data.frame(results)
		names(results)[2] = 'Item'
	} else {
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
		
		results = cbind(row.names(results), results)
		names(results)[1] = 'Item'
		row.names(results) = 1:nrow(results) 		
	}
	
	r = list(results=results, items=items, grouping=grouping, nlevels=nlevels)
	class(r) = 'likert'
	return(r)
}
