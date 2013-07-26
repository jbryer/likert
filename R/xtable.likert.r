#' Prints a LaTeX table of the likert items.
#' 
#' @param x likert class object.
#' @param caption the table caption.
#' @param label the table label.
#' @param align column alignments.
#' @param digits number of digits to use for numeric columns.
#' @param display column formats.
#' @param include.n option to include n
#' @param include.mean option to include mean
#' @param include.sd option to include sd
#' @param include.low option to include low
#' @param include.neutral option to include neutral
#' @param include.high option to include high
#' @param include.levels option to include levels
#' @param include.missing option to include missing levels.
#' @param ... other parameters passed to \link{xtable}.
#' @seealso \link{xtable}, \link{print.xtable}
#' @S3method xtable likert
#' @method xtable likert
xtable.likert <- function(x, caption=NULL, label=NULL, align=NULL, digits=NULL,
                          display=NULL, include.n=TRUE, include.mean=TRUE, include.sd=TRUE, 
                          include.low=TRUE, include.neutral=TRUE, include.high=TRUE, 
                          include.levels=TRUE, include.missing=TRUE, 
                          center=(x$nlevels-1)/2 + 1, ordered=TRUE,...) {
  if(!is.null(x$grouping)){
    
  }else{
    s<-summary(x, center=center,ordered=ordered) 
    tab<-as.data.frame(as.character(s$Item))
    names(tab)<-'Item'
    if(include.n){tab<-cbind(tab, rep(nrow(x$items),nrow(x$results)))
                  names(tab)<-c(names(tab[1:ncol(tab)-1]),'n')}
    if(include.mean){tab<-cbind(tab, s$mean)
                     names(tab)<-c(names(tab[1:ncol(tab)-1]),'mean')}
    if(include.sd){tab<-cbind(tab, s$sd)
                   names(tab)<-c(names(tab[1:ncol(tab)-1]),'sd')}
    if(include.low){tab<-cbind(tab,s$low)
                    names(tab)<-c(names(tab[1:ncol(tab)-1]),'low')}
    if(include.neutral){tab<-cbind(tab, s$neutral)
                        names(tab)<-c(names(tab[1:ncol(tab)-1]),'neutral')}
    if(include.high){tab<-cbind(tab, s$high)
                     names(tab)<-c(names(tab[1:ncol(tab)-1]),'high')}
    if(include.missing){
      missing<-as.numeric()
      for(i in 1:ncol(x$items)){
        missing<- c(missing, prop.table(table(is.na(x$items[i])))[2]*100)
      }
      names(missing)<-NULL
      tab<-cbind(tab, missing)
      names(tab)<-c(names(tab[1:ncol(tab)-1]),'missing')
    }
    #caption=paste0('For these items, there were:',x$nlevels,'response categories including:',x$items$levels)#todo sep levels with commas etc
    #TODO include.levels
    xtab<-xtable(tab, caption=caption, label=label, align=align, digits=digits,
                 display=display)
  }
  return(xtab)
}
