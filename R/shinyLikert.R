#' Shiny App for the likert package.
#' 
#' This will start a shiny app included with the package to show many of the features
#' available in the likert package.
#' 
#' @references http://rstudio.com/shiny
shinyLikert <- function() {
	shiny::runApp(system.file('shiny', package='likert'))
}
