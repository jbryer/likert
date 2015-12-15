##### START zeroGrob FROM ggplot2 ##############################################
# Copied from ggplot2. This is a non-exported function. To avoid R CMD CHECK
# note for use ::: the functions were copied here.

#' The zero grob draws nothing and has zero size.
#' 
#' @keywords internal
#' @author hadley
#' @references https://github.com/hadley/ggplot2/blob/master/R/grob-null.r
zeroGrob <- function() .zeroGrob

.zeroGrob <- grob(cl = "zeroGrob", name = "NULL")
widthDetails.zeroGrob <-
	heightDetails.zeroGrob <- 
	grobWidth.zeroGrob <- 
	grobHeight.zeroGrob <- function(x) grid::unit(0, "cm")

drawDetails.zeroGrob <- function(x, recording) {}

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")

##### END zeroGrob FROM ggplot2 ################################################

#' Adapted from ggExtra package which is no longer available. This is related to
#' an experimental mlpsa plot that will combine the circular plot along with
#' the two individual distributions.
#' 
#' @param gl grid.layout
#' @param ... graphic elements to combine.
#' @references 	http://groups.google.com/group/ggplot2/browse_thread/thread/1b859d6b4b441c90
#'              http://ggextra.googlecode.com/svn/trunk/R/align.r
align.plots <- function(gl, ...) {
	#With ggplot2 version .9.2 these functions were removed. This is a bit of
	#a hack copying them from an older version, but seems to still work!
	ggplotGrob <- function (x) {
		gtable_gTree(ggplot_gtable(ggplot_build(x)))
	}
	
	gtable_gTree <- function (x, ...) {
		children <- gtable_gList(x)
		vp <- gtable_viewport(x)
		gTree(children = children, childrenvp = vp, ...)
	}
	
	gtable_gList <- function (x) {
		names <- with(x$layout, paste(name, t, l, sep = "-"))
		grobs <- llply(seq_along(names), function(i) {
			editGrob(x$grobs[[i]], vp = vpPath(x$name, names[i]), 
					 name = names[i])
		})
		do.call("gList", grobs)
	}
	
	gtable_viewport <- function (x) {
		layout_vp <- viewport(layout = gtable_layout(x), name = x$name)
		vp <- function(i) {
			vp <- x$layout[i, ]
			viewport(name = paste(vp$name, vp$t, vp$l, sep = "-"), 
					 layout.pos.row = vp$t:vp$b, layout.pos.col = vp$l:vp$r, 
					 clip = vp$clip)
		}
		children_vp <- do.call("vpList", llply(seq_along(x$grobs), 
											   vp))
		vpTree(layout_vp, children_vp)
	}
	
	gtable_layout <- function (x) {
		grid.layout(nrow = nrow(x), heights = x$heights, ncol = ncol(x), 
					widths = x$widths, respect = x$respect)
	}

	# Obtained from http://groups.google.com/group/ggplot2/browse_thread/thread/1b859d6b4b441c90
	# Adopted from http://ggextra.googlecode.com/svn/trunk/R/align.r
	
	# BUGBUG: Does not align horizontally when one has a title.
	#    There seems to be a spacer used when a title is present.  Include the
	#    size of the spacer.  Not sure how to do this yet.
	
	stats.row <- vector( "list", gl$nrow )
	stats.col <- vector( "list", gl$ncol )
	
	lstAll <- list(...)
	
	dots <- lapply(lstAll, function(.g) {
			ggplotGrob(.g[[1]]) })
	
	plottitles <- lapply(dots, function(.g) {
				if(!is.null(getGrob(.g, 'plot.title.text', grep=TRUE)))
					editGrob(getGrob(.g, "plot.title.text", grep=TRUE), vp=NULL) 
				else .zeroGrob
				})
	
	xtitles <- lapply(dots, function(.g) {
				#.g <- ggplotGrob(.g)
				if(!is.null(getGrob(.g, "axis.title.x.text", grep=TRUE)))
					editGrob(getGrob(.g,"axis.title.x.text",grep=TRUE), vp=NULL) 
				else .zeroGrob
				})   
	
	xlabels <- lapply(dots, function(.g) {
				#.g <- ggplotGrob(.g)
				if(!is.null(getGrob(.g, "axis.text.x.text",grep=TRUE)))
					editGrob(getGrob(.g,"axis.text.x.text",grep=TRUE), vp=NULL) 
				else .zeroGrob
				})  
	
	ytitles <- lapply(dots, function(.g) {
				#.g <- ggplotGrob(.g)
				if(!is.null(getGrob(.g,"axis.title.y.text",grep=TRUE)))
					editGrob(getGrob(.g,"axis.title.y.text",grep=TRUE), vp=NULL) 
				else .zeroGrob
				})
	
	ylabels <- lapply(dots, function(.g) {
				#.g <- ggplotGrob(.g)
				if(!is.null(getGrob(.g,"axis.text.y.text",grep=TRUE)))
					editGrob(getGrob(.g,"axis.text.y.text",grep=TRUE), vp=NULL) 
				else .zeroGrob
				})
	
	legends <- lapply(dots, function(.g) {
				if(!is.null(.g$children$legends))
					editGrob(.g$children$legends, vp=NULL) 
				else .zeroGrob
				})
	
	widths.left <- mapply(`+`, e1=lapply(ytitles, grobWidth),
						  e2= lapply(ylabels, grobWidth), SIMPLIFY=FALSE)
	widths.right <- lapply(legends, grobWidth)
	#  heights.top <- lapply(plottitles, grobHeight)
	heights.top <- lapply( plottitles, function(x) grid::unit(0,"cm") )
	heights.bottom <- mapply(`+`, e1=lapply(xtitles, grobHeight), 
							 e2=lapply(xlabels, grobHeight), SIMPLIFY=FALSE)
	
	for ( i in seq_along( lstAll ) ) {
		lstCur <- lstAll[[i]]
		
		# Left
		valNew <- widths.left[[ i ]]
		valOld <- stats.col[[ min(lstCur[[3]]) ]]$widths.left.max
		if ( is.null( valOld ) ) valOld <- grid::unit( 0, "cm" )
		stats.col[[ min(lstCur[[3]]) ]]$widths.left.max <- 
			max( do.call( grid::unit.c, list(valOld, valNew) ) )
		
		# Right
		valNew <- widths.right[[ i ]]
		valOld <- stats.col[[ max(lstCur[[3]]) ]]$widths.right.max
		if ( is.null( valOld ) ) valOld <- grid::unit( 0, "cm" )
		stats.col[[ max(lstCur[[3]]) ]]$widths.right.max <- 
			max( do.call( grid::unit.c, list(valOld, valNew) ) )
		
		# Top
		valNew <- heights.top[[ i ]]
		valOld <- stats.row[[ min(lstCur[[2]]) ]]$heights.top.max
		if ( is.null( valOld ) ) valOld <- grid::unit( 0, "cm" )
		stats.row[[ min(lstCur[[2]]) ]]$heights.top.max <- 
			max( do.call( grid::unit.c, list(valOld, valNew) ) )
		
		# Bottom
		valNew <- heights.bottom[[ i ]]
		valOld <- stats.row[[ max(lstCur[[2]]) ]]$heights.bottom.max
		if ( is.null( valOld ) ) valOld <- grid::unit( 0, "cm" )
		stats.row[[ max(lstCur[[2]]) ]]$heights.bottom.max <- 
			max( do.call( grid::unit.c, list(valOld, valNew) ) )
	}
	
	for(i in seq_along(dots)){
		lstCur <- lstAll[[i]]
		nWidthLeftMax <- stats.col[[ min( lstCur[[ 3 ]] ) ]]$widths.left.max
		nWidthRightMax <- stats.col[[ max( lstCur[[ 3 ]] ) ]]$widths.right.max
		nHeightTopMax <- stats.row[[ min( lstCur[[ 2 ]] ) ]]$heights.top.max
		nHeightBottomMax <- stats.row[[ max( lstCur[[ 2 ]] ) ]]$heights.bottom.max
		pushViewport( viewport( layout.pos.row=lstCur[[2]],
								layout.pos.col=lstCur[[3]], just=c("left","top") ) )
		pushViewport(viewport(
			x=grid::unit(0, "npc") + nWidthLeftMax - widths.left[[i]],
			y=grid::unit(0, "npc") + nHeightBottomMax - heights.bottom[[i]],
			width=grid::unit(1, "npc") - nWidthLeftMax + widths.left[[i]] -
				nWidthRightMax + widths.right[[i]],
			height=grid::unit(1, "npc") - nHeightBottomMax + heights.bottom[[i]] -
				nHeightTopMax + heights.top[[i]],
			just=c("left","bottom")))
		grid.draw(dots[[i]])
		upViewport(2)
	}
}
