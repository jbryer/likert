#'Takes a list of dataframe column names, and long names, and stores the long names in the attribute "fullname",
#'which can later be accessed by R.
#'
#'Example:
#'db <- add_likert_fullnames(db, c(
#'  "X7"="Do you use sites like Facebook, Twitter, or GPlus?",
#   "X8"="Do you participate in online communities organised around your interests?",
#'  "X10"="Do you know of online communities relevant to your discipline or the courses you are taking now?"))

likert_add_fullnames <- function(to, fnames) {
  
  if(length(fnames) > length(unique(fnames))) {
  	stop("All names must be unique")
  }

  for(x in names(fnames)) {
    attr(to[[x]], "fullname") <- fnames[[x]]
  }
  to
}