library(shiny)
library(colourpicker)
library(likert)

data(pisaitems)
data(mass)

items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']

names(items24) <- c(
	"I read only if I have to.",
	"Reading is one of my favorite hobbies.",
	"I like talking about books with other people.",
	"I find it hard to finish books.",
	"I feel happy if I receive a book as a present.",
	"For me, reading is a waste of time.",
	"I enjoy going to a bookstore or a library.",
	"I read only to get information that I need.",
	"I cannot sit still and read for more than a few minutes.",
	"I like to express my opinions about books I have read.",
	"I like to exchange books with my friends.")

items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")

# Datasets that will be available in the Shiny app
datasets <- list(
	pisa_reading_attitude = list(
		data = items24,
		groupings = pisaitems[,c('CNT'), drop=FALSE],
		name = 'Reading Attitudes (PISA)'
	),
	
	pisa_reading_frequency = list(
		data = items29,
		groupings = pisaitems[,c('CNT'), drop=FALSE],
		name = 'Reading Frequency (PISA)'
	),
	
	math_anxiety = list(
		data = mass[,2:15],
		groupings = mass[,'Gender',drop=FALSE],
		name = 'Math Anxiety Scale Survey'
	)
)