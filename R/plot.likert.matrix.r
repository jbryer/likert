#Tal Galili
#http://www.r-statistics.com/2010/04/correlation-scatter-plot-matrix-for-ordered-categorical-data/

panel.cor.ordered.categorical <- function(x, y, digits=2, prefix="", cex.cor) {
	usr <- par("usr"); on.exit(par(usr)) 
	par(usr = c(0, 1, 0, 1)) 
	
	# notive we use spearman, non parametric correlation here
	r <- abs(cor(x, y, method = "spearman", use='pairwise.complete.obs'))
	r.no.abs <- cor(x, y, method = "spearman", use='pairwise.complete.obs')
	
	txt <- format(c(r.no.abs , 0.123456789), digits=digits)[1] 
	txt <- paste(prefix, txt, sep="") 
	if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
	
	test <- cor.test(x,y, method = "spearman") 
	# borrowed from printCoefmat
	Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
					 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
					 symbols = c("***", "**", "*", ".", " ")) 
	
	text(0.5, 0.5, txt, cex = cex) 
	text(.8, .8, Signif, cex=cex, col=2) 
}

panel.smooth.ordered.categorical <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
											  cex = 1, col.smooth = "red", span = 2/3, iter = 3, 
											  point.size.rescale = .8, ...) {
	#Remove missing values
	xmiss = is.na(x)
	x = x[!xmiss]
	y = y[!xmiss]
	ymiss = is.na(y)
	x = x[!ymiss]
	y = y[!ymiss]
	
	z <- merge(data.frame(x,y), reshape2::melt(table(x,y)), sort=F)$value
	z <- point.size.rescale*z/ (length(x)) # notice how we rescale the dots accourding to the maximum z could have gotten
	
	symbols(x, y,  circles = z,#rep(0.1, length(x)), #sample(1:2, length(x), replace = T) ,
			 inches=F, bg= "grey",#the.col ,
			 fg = bg, add = T)
	
	# points(x, y, pch = pch, col = col, bg = bg, cex = cex)
	ok <- is.finite(x) & is.finite(y)
	if (any(ok)) 
		lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
}

panel.hist <- function(x, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE, br = 20)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col="orange", ...)
}


pairs.ordered.categorical <- function(xx,...) {
	pairs(xx, 
		  diag.panel = panel.hist ,
		  lower.panel=panel.smooth.ordered.categorical,
		  upper.panel=panel.cor.ordered.categorical,
		  cex.labels = 1.5, ...) 
}

#' Matrix plot (experimental)
#' 
#' @param likert results of \code{\link{likert}}.
#' @param nSample random sample of all rows. This function may take a while
#'        to run with large datasets (including the \code{pisaitems} data). Plotting
#'        a random subsample allows for quicker development.
#' @param ... parameters passed to \code{pairs.ordered.categorical}.
likert.matrix.plot <- function(likert, nSample=nrow(likert$items), ...) {
	rows = sample.int(nrow(likert$items), nSample)
	pairs.ordered.categorical(likert$items[rows,], ...)
}
