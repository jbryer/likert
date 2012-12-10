install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)
require(roxygen2)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/Projects") #Windows

#Package building
document("likert")
check_doc("likert")
build("likert", binary=FALSE)
build("likert", binary=TRUE)
install("likert")
check("likert")
library(likert)
ls('package:likert')

