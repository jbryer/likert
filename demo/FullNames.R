options(digits=2)

require(likert)
data(pisaitems)

# adding metadata to pisaitems, including a reading group, and full names
# this will be stored together with the pisaitems object, and can be used later
attr(pisaitems, "reading") <- c(
	"ST24Q01",
	"ST24Q02",
	"ST24Q03",
	"ST24Q04",
	"ST24Q05",
	"ST24Q06",
	"ST24Q07",
	"ST24Q08",
	"ST24Q09",
	"ST24Q10",
	"ST24Q11")

pisaitems <- add_likert_fullnames(pisaitems, c(
	ST24Q01="I read only if I have to.",
	ST24Q02="Reading is one of my favorite hobbies.",
	ST24Q03="I like talking about books with other people.",
	ST24Q04="I find it hard to finish books.",
	ST24Q05="I feel happy if I receive a book as a present.",
	ST24Q06="For me, reading is a waste of time.",
	ST24Q07="I enjoy going to a bookstore or a library.",
	ST24Q08="I read only to get information that I need.",
	ST24Q09="I cannot sit still and read for more than a few minutes.",
	ST24Q10="I like to express my opinions about books I have read.",
	ST24Q11="I like to exchange books with my friends."))

# using the group reading to select the group of questions we want to plot
l24 <-pisaitems[,attr(pisaitems, "reading")]

# directly plotting, the full names have already been stored
plot(likert(l24))