#'Stores metadata about groups into a data frame, for future use by plot_likert_group.
#'Example usage (X7 etc are column names, titles are used as titles for group plots)
#'
#'groups <- list(
#'  "Use of social media"=c("X7","X8","X10","X11","X12"), 
#'  "Social learning"=c("X13","X14","X15","X16","X17","X18","X19","X20"), 
#'  "Connection with other students"=c("X21","X22","X23","X24","X25"),
#'  "Interaction with professors"=c("X26","X27","X28","X29","X30","X31"), 
#'  "Experience of your major"='c("X32","X33","X34","X35","X36""), 
#'  "Identification with your discipline/major"=c("X40","X41","X42"))
#'
#' db <- likert_store_groups(db, groups)

likert_store_groups <- function(db, groups) {
  attr(db, "likert.groups") <- groups
  db 
}