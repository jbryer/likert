library(devtools)

#Package building
document()
install()
build()
test()
#Rbuild('likert',vignettes=FALSE)
build_vignettes()
check(cran=TRUE)

# Build website
pkgdown::build_site()

# Ready for CRAN?
release()

##### Basic testing
library(likert)
ls('package:likert')

#Run shiny app. See also shinyLikert to run from the installed package.
shiny::runApp('likert/inst/shiny')

##### testthat
usethis::use_testthat()
usethis::use_test('duplicate_gruops_124')

##### Data setup. We will use a few of the student items from North America PISA
require(pisa)
data(pisa.student)
pisaitems <- pisa.student[,substr(names(pisa.student), 1,5) %in% 
				c('CNT', #Country
				  'ST24Q', #Read attitude
				  'ST25Q', #Like reading
				  'ST26Q', #Online
				  'ST27Q', #Study
				  'ST34Q', #Teachers
				  'ST36Q', #Lessons
				  'ST37Q', #Stimulate
				  'ST38Q', #Strategies
				  'ST39Q', #Library
				  'ST41Q', #Text
				  'ST42Q', #Summary
				  paste('PV', 1:5, 'MATH', sep=''),
				  paste('PV', 1:5, 'READ', sep=''),
				  paste('PV', 1:5, 'SCIE', sep='')
)]
pisaitems <- pisaitems[pisaitems$CNT %in% c('Canada','Mexico','United States'),]
pisaitems$CNT <- as.factor(as.character(pisaitems$CNT))
names(pisaitems); nrow(pisaitems); ncol(pisaitems)
save(pisaitems, file='likert/data/pisaitems.rda')
tools::resaveRdaFiles('likert/data/pisaitems.rda')


