#' Wrap label text.
#' 
#' @param value vector (converted using \code{\link{as.character}}) to be wrapped.
#' @param width the maximum width of each line in characters.
#' 
#' Adapted from https://github.com/hadley/ggplot2/wiki/labeller
label_wrap_mod <- function(value, width = 25) {
  sapply(strwrap(as.character(value), width=width, simplify=FALSE), 
         paste, collapse="\n")
}
