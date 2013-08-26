#'Takes a list of dataframe column names, and long names, and stores the long names in the attribute "fullname",
#'which can later be accessed by R.

add_likert_fullnames <- function(to, fnames) {
  for(x in names(fnames)) {
    attr(to[[x]], "fullname") <- fnames[[x]]
  }
  to
}