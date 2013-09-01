#'Plots a group of Likert-items, as stored by likert_store_groups. groups= can be one group, indexed by title or by number
#'or a list of groups. all=T prints all groups. 

plot_likert_groups <- function(db, all=F, groups=NA, ...) {
  attrgroups <- attr(db, "likert.groups")
  
  if(is.null(attrgroups)) {
    stop("You have not stored any groups using likert_store_groups")
  }

  if(all) { 
    groups <- names(attrgroups) 
  }

  if(all(is.na(groups))) {
    stop("You have not specified a group name using groups=, or all=F")
  }
  
  for(e in groups) {
    group <- attrgroups[[e]]
    ligroup <- likert(db[,group], ...)
    print(plot(ligroup) + ggtitle(names(attrgroups[e])) )
  }
  
}