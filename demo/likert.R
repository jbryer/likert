options(digits=2)

data(pisaitems)

items28 = pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
head(items28); ncol(items28)
items28 <- rename(items28, c(
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
			ST24Q11="I like to exchange books with my friends"))
# for(i in 1:ncol(items28)) {
# 	items28[,i] = factor(items28[,i], levels=1:4, 
# 						 labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'), 
# 						 ordered=TRUE)
# }
str(items28)

l28 = likert(items28)
print(l28)
summary(l28)

plot(l28)
plot(l28, centered=TRUE, low.color='#FF9900', high.color='#660066')

plot(l28, type='heat')

#Experimental.
#plot.likert.matrix(l28)

##### Group by Country
l28 = likert(items28, grouping=pisaitems$CNT)
print(l28)
summary(l28)

plot(l28)
plot(l28, centered=TRUE, low.color='#FF9900', high.color='#660066')


#Group by country
l28g = likert(items28, grouping = pisana$CNT)
print(l28g)
summary(l28g)

plot(l28g, low.color='maroon', high.color='burlywood4')
plot(l28g, low.color='maroon', high.color='burlywood4', centered=TRUE)

#How often do you read these materials because you want to?
items29 = pisaitems[,substr(names(pisana), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
for(i in 1:ncol(items29)) {
	items29[,i] = factor(items29[,i], levels=1:5, 
						 labels=c('Never or almost never', 'A few times a year', 
						 		 'About once a month', 'Several times a month', 
						 		 'Several times a week'), ordered=TRUE)
}

l29 = likert(items29)
print(l29)
summary(l29)

plot(l29, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")

plot(l29, type='heat') + opts(title="How often do you read these materials because you want to?")

l29g = likert(items29, grouping=pisana$CNT)
summary(l29g)

plot(l29g, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")

plot(l29g, centered=TRUE, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")
