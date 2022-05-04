#' Prints a LaTeX table of the likert items.
#' 
#' Crate a LaTeX or HTML table of the \code{\link{likert}} results.
#' 
#' @param x likert class object.
#' @param caption the table caption.
#' @param label the table label.
#' @param align column alignments.
#' @param digits number of digits to use for numeric columns.
#' @param display column formats.
#' @param auto Logical, indicating whether to apply automatic format when no 
#'        value is passed to align, digits, or display (see \code{\link{xtable}}
#'        for more information.
#' @param include.n option to include n
#' @param include.mean option to include mean
#' @param include.sd option to include sd
#' @param include.low option to include low
#' @param include.neutral option to include neutral
#' @param include.high option to include high
#' @param include.levels option to include levels
#' @param include.missing option to include missing levels.
#' @param center specifies which level should be treated as the center. For example,
#'        \code{center = 3} would use the third level as the center whereas
#'        \code{center = 3.5} would indicate no specific level is the center but
#'        <= 3 are low levels and >= 4 are high levels (i.e. used for forced choice 
#'        items or those without a neutral option). This also influences which levels
#'        are summarized in the low and high groups.
#' @param ordered whether the results should be ordered. See \code{\link{summary.likert}}
#' @param ... other parameters passed to \link{xtable}.
#' @seealso \link{xtable}, \link{print.xtable}
#' @export
#' @method xtable likert
xtable.likert <- function(x, caption=NULL, 
						  label=NULL, 
						  align=NULL, 
						  digits=NULL,
                          display=NULL, 
						  auto=FALSE,
						  include.n=TRUE, 
						  include.mean=TRUE, 
						  include.sd=TRUE, 
                          include.low=TRUE, 
						  include.neutral=(x$nlevels %% 2 != 0), 
						  include.high=TRUE, 
                          include.levels=TRUE, 
						  include.missing=TRUE, 
                          center=(x$nlevels-1)/2 + 1, 
						  ordered=TRUE,
						  ...) {
	if(!is.null(x$grouping)) {
		tab <- data.frame()
		for(g in unique(x$results$Group)){
			s <- summary(x, center=center,ordered=ordered) 
			s <- s[which(s$Group==g),]
			gtab <- as.data.frame(cbind(as.character(s$Group),as.character(s$Item)))
			names(gtab) <- c('Group','Item')
			missing <- as.numeric()
			for(i in 1:ncol(x$items)){
				missing <-  c(missing, prop.table(table(is.na(x$items[i])))[2])
			}
			names(missing) <- NULL
			if(include.n) {
				gtab <- cbind(gtab, rep(nrow(x$items),length(x$items)))
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'n')
				gtab$n <- as.integer(gtab$n-(gtab$n*missing))}
			if(include.mean) {
				gtab <- cbind(gtab, s$mean)
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'mean')}
			if(include.sd) {
				gtab <- cbind(gtab, s$sd)
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'sd')
			}
			if(include.low) {
				gtab <- cbind(gtab,s$low)
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'low')
			}
			if(include.neutral) {
				gtab <- cbind(gtab, s$neutral)
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'neutral')
			}
			if(include.high) {
				gtab <- cbind(gtab, s$high)
				names(gtab) <- c(names(gtab[1:ncol(gtab)-1]),'high')
			}
			tab <- rbind(tab,gtab) 
			#       hline  <-  c(-1,0, nrow(tab))
		}
	} else {
		s  <-  summary(x, center=center, ordered=ordered)
		tab <- as.data.frame(as.character(s$Item))
		names(tab) <- 'Item'
		missing <- as.numeric()
		for(i in 1:ncol(x$items)) {
			missing <-  c(missing, prop.table(table(is.na(x$items[i])))[2])
		}
		names(missing) <- NULL
		if(include.n) {
			tab <- cbind(tab, rep(nrow(x$items),nrow(x$results)))
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'n')
			tab$n <- as.integer(tab$n-(tab$n*missing))
		}
		if(include.mean) {
			tab <- cbind(tab, s$mean)
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'mean')
		}
		if(include.sd) {
			tab <- cbind(tab, s$sd)
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'sd')
		}
		if(include.low) {
			tab <- cbind(tab,s$low)
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'low')
		}
		if(include.neutral) {
			tab <- cbind(tab, s$neutral)
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'neutral')
		}
		if(include.high) {
			tab <- cbind(tab, s$high)
			names(tab) <- c(names(tab[1:ncol(tab)-1]),'high')
		}
		#     hline <- c(-1,0,nrow(tab))
    }
    #caption=paste0('For these items, there were:',x$nlevels,'response categories including:',x$items$levels)#todo sep levels with commas etc
    #TODO include.levels
  #TODO: align should be defined, not passed through
    xtab <- xtable(tab, caption=caption, label=label, align=align, digits=digits,
                 display=display, auto=auto, include.rownames=FALSE)
    class(xtab) <- c('xlikert',class(xtab))
	return(xtab)
}

#' Prints the results of \code{\link{xtable.likert}}.
#' 
#' Print method for \code{\link{xtable.likert}}.
#' 
#' @param x results of \code{\link{xtable.likert}}.
#' @param tabular.environment see \code{\link{print.xtable}}.
#' @param floating see \code{\link{print.xtable}}.
#' @param ... other parameters passed to \code{\link{print.xtable}}
#' @method print xlikert
#' @export
print.xlikert <- function(x, tabular.environment='longtable', floating=FALSE, ...) {
	if(is.null(x$Group)) {
		hlineafter <- c(-1,0,nrow(x))
	} else {
		ng<-nlevels(x$Group)
		ni<-nrow(x)/ng
		hlineafter <- c(-1,0,seq(from=ni, to=ni*ng, by=ni))
	}
	print.xtable(x, floating=floating, include.rownames=FALSE, 
				 include.colnames=TRUE, hline.after=hlineafter, ...)
}
