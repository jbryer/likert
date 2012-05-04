#' Plots a set of likert items. 
#'
#' @param likert the likert items to plot
#' @param low.color color corresponding to the lowest value likert items
#' @param high.color color corresponding to the highest value likert items
#' @param neutral.color color for middle values. Only used when there are an odd
#'        number of levels.
#' @param text.size size or text labels
#' @param text.color the color of text in heat map cells
#' @param type whether to plot a bar or heat map graphic
#' @export
#' @method plot likert
#' @S3method plot likert
plot.likert <- function(likert, text.size=2, type=c('bar','heat'), ...) {
	if(type[1] == 'bar') {
		plot.likert.bar(likert, 
						#low.color=low.color, high.color=high.color, neutral.color=neutral.color, 
						text.size=text.size, ...)
	} else {
		plot.likert.heat(likert, #low.color=low.color, high.color=high.color,
						 text.size=text.size, ...)
	}
}
