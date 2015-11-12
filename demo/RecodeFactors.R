mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')

# Recode example
test1 <- factor(c(1,2,3,5))
test2 <- factor(c(1,2,3,5), levels=1:5)
cbind(as.character(test1), 
	  as.integer(test1), 
	  as.integer(test2),
	  recode(test2, from=1:5, to=5:1), # One approach to reverse coding
	  recode(test2, from=1:5, to=mylevels) # To better labels
)

# Reverse code
data(pisaitems)

##### PISA Item 24: Reading Attitudes
data(pisaitems)
items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
names(items24) <- c(
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
	ST24Q11="I like to exchange books with my friends.")

(l.orignal <- likert(items24))
levels(items24[,1])
table(items24[,1])
plot(l.orignal)

items24.reverse <- reverse.levels(items24)
levels(items24.reverse[,1])
table(items24.reverse[,1])
(l.reverse <- likert(items24.reverse))
plot(l.reverse)
