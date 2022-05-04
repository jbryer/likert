#' Reverse the levels of a factor.
#' 
#' @param x a factor or a data.frame of factors whose levels will be reverse coded.
#' @export
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#' test <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#' cbind(test, as.integer(test), as.integer(reverse.levels(test)))
reverse.levels <- function(x) {
	if(is.factor(x)) {
		x <- factor(as.character(x), levels=rev(levels(x)), ordered=TRUE)
	} else if(is.data.frame(x)) {
		for(i in seq_along(x)) {
			if(is.factor(x[,i])) {
				x[,i] <- factor(as.character(x[,i]), levels=rev(levels(x[,i])), ordered=TRUE)
			} else {
				warning(paste0('Column ', i, ' is not a factor.'))
			}
		}
	} else {
		stop(paste0('Unsupported format: ', class(x)))
	}
	return(x)
}
