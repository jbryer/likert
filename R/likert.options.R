
#' Builds an object with options for plotting \code{\link{likert}} results.
#' 
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
#' @param ... included for future expansion.
#'        
#' @export
likert.options <- function(
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
	include.center=TRUE,
	ordered=TRUE,
	wrap=50,
	wrap.grouping=50,
	legend='Response',
	legend.position='bottom',
	panel.arrange='v',
	panel.strip.color='#F0F0F0',
	...
) {
	opts <- list(
		low.color = low.color,
		high.color = high.color,
		neutral.color = neutral.color,
		neutral.color.ramp = neutral.color.ramp,
		colors = colors,
		plot.percent.low = plot.percent.low,
		plot.percent.high = plot.percent.high,
		plot.percent.neutral = plot.percent.neutral,
		plot.percents = plot.percents,
		text.size = text.size,
		text.color = text.color,
		centered = centered,
		include.center = include.center,
		ordered = ordered,
		wrap = wrap,
		wrap.grouping = wrap.grouping,
		legend = legend,
		legend.position = legend.position,
		panel.arrange = panel.arrange,
		panel.strip.color = panel.strip.color
	)
	
	return(opts)
}
