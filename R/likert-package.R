#' Likert Analysis and Visualization
#' 
#' @name likert-package
#' @docType package
#' @title Likert Analysis and Visualization
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research likert
#' @import xtable
#' @import ggplot2
#' @import gridExtra
#' @import reshape2
#' @importFrom grid grob editGrob vpPath viewport vpTree grid.layout getGrob gTree
#'             grobWidth grobHeight pushViewport grid.draw upViewport grid.newpage
#' @importFrom psych describe describeBy
#' @importFrom grDevices colorRamp rgb
#' @importFrom graphics hist lines pairs par rect strwidth symbols text
#' @importFrom stats cor cor.test density sd symnum
#' @importFrom plyr llply
NA

#' Fictitious dataset with importance and satisfaction results across five different
#' offices.
#' 
#' This data set is used in the \code{GapAnalysis} demo and is used to demonstrate
#' how the \code{likert} package handles a gap analysis.
#' 
#' @name gap
#' @docType data
#' @format a data frame with 68 ovservations of 11 variables.
#' @keywords datasets
NA

#' Results from the Survey of Academic Self-Regulation (SASR).
#' 
#' The Survey of Academic Self-Regulation (SASR) is comprised of six factors:
#' self-regulation, intrinsic motivation, extrinsic motivation, self-efficacy,
#' metacognition, and personal relevance and control.
#' 
#' @name sasr
#' @docType data
#' @format a data frame with 860 ovservations of 63 variables.
#' @keywords datasets
#' @references Dugan, R., & Andrade, H. (2011). Exploring the construct validity 
#' of academic self-regulation using a new self-report questionnaire. The 
#' International Journal of Educational and Psychological Assessment, 7(1).
NA

#' Programme of International Student Assessment
#' 
#' North American (i.e. Canada, Mexico, and United States) results from the 2009
#' Programme of International Student Assessment (PISA)
#' as provided by the Organization for Economic Co-operation and Development (OECD).
#' See \url{http://www.pisa.oecd.org/} for more information including the code book.
#'
#' @name pisaitems
#' @docType data
#' @format a data frame 66,690 ovservations of 81 variables from North America.
#' @source Organization for Economic Co-operation and Development
#' @keywords datasets
NA

#' Results from an administration of the Math Anxiety Scale Survey.
#' 
#' A data frame of results of the Math Anxiety Scale Survey administered
#' to 20 students in a statistics course. This data frame contains the original
#' data and can be used to verify the pre-summarized procedures.
#' 
#' @name mass
#' @docType data
#' @format data frame with 14 rows and 6 columns.
#' @references Bai, H., Wang, L., Pan, W., & Frey, M. (2009). Measuring mathematics 
#' anxiety: Psychometric analysis of a bidimensional affective scale. Journal of 
#' Instructional Psychology, 36 (3), 185- 193.
#' @keywords datasets
NA

#' Pre-summarized results from an administration of the Math Anxiety Scale Survey.
#' 
#' A data frame of presummarized results of the Math Anxiety Scale Survey administered
#' to 20 students in a statistics course.
#' 
#' @name MathAnxiety
#' @docType data
#' @format data frame with 14 rows and 6 columns.
#' @references Bai, H., Wang, L., Pan, W., & Frey, M. (2009). Measuring mathematics 
#' anxiety: Psychometric analysis of a bidimensional affective scale. Journal of 
#' Instructional Psychology, 36 (3), 185- 193.
#' @keywords datasets
NA

#' Pre-summarized results from an administration of the Math Anxiety Scale Survey 
#' grouped by gender.
#' 
#' A data frame of presummarized results of the Math Anxiety Scale Survey administered
#' to 20 students in a statistics course grouped by gender.
#' 
#' @name MathAnxietyGender
#' @docType data
#' @format data frame with 28 rows and 7 columns.
#' @references Bai, H., Wang, L., Pan, W., & Frey, M. (2009). Measuring mathematics 
#' anxiety: Psychometric analysis of a bidimensional affective scale. Journal of 
#' Instructional Psychology, 36 (3), 185- 193.
#' @keywords datasets
NA

.onAttach <- function(libname, pkgname) {
	#pkgEnv = pos.to.env(match('package:likert', search()))
	#assignInNamespace("sqlrepos", paste(system.file(package='likert'), '/data', sep=''), "irutils")
	if('package:reshape' %in% search()) {
		packageStartupMessage(
"The reshape package was found on the search path. It is recommended
that you detach this package as it causes conflicts with the reshape2
package. You can detach using the following R command:
    detach('package:reshape') ")
	}
}
