theme_update(panel.background=theme_blank(), 
			 panel.grid.major=theme_blank(), 
			 panel.border=theme_blank())

data(pisana)

items28 = pisana[,substr(names(pisana), 1,5) == 'ST24Q']
head(items28); ncol(items28)
names(items28) = c("I read only if I have to.",
				   "Reading is one of my favorite hobbies.",
				   "I like talking about books with other people.",
				   "I find it hard to finish books.",
				   "I feel happy if I receive a book as a present.",
				   "For me, reading is a waste of time.",
				   "I enjoy going to a bookstore or a library.",
				   "I read only to get information that I need.",
				   "I cannot sit still and read for more than a few minutes.",
				   "I like to express my opinions about books I have read.",
				   "I like to exchange books with my friends")
for(i in 1:ncol(items28)) {
	items28[,i] = factor(items28[,i], levels=1:4, 
						 labels=c('Strongly disagree', 'Disagree', 'Agree', 'Strongly Agree'), 
						 ordered=TRUE)
}

l28 = likert(items28)
l28
summary(l28)

plot(l28)
plot(l28, centered=TRUE)

plot(l28, type='heat')

plot.likert.matrix(l28)

tmp = l28$items
for(i in seq_len(ncol(tmp))) { tmp[,i] = as.integer(tmp[,i]) }
tmp = cor(tmp, use='pairwise.complete.obs')
View(tmp)

#Group by country
l28g = likert(items28, grouping = pisana$CNT)
plot(l28g, low.color='maroon', high.color='burlywood4')
plot(l28g, low.color='maroon', high.color='burlywood4', centered=TRUE)

#How often do you read these materials because you want to?
items29 = pisana[,substr(names(pisana), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
for(i in 1:ncol(items29)) {
	items29[,i] = factor(items29[,i], levels=1:5, 
						 labels=c('Never or almost never', 'A few times a year', 
						 		 'About once a month', 'Several times a month', 
						 		 'Several times a week'), ordered=TRUE)
}

l29 = likert(items29)
summary(l29)

plot(l29, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")

plot(l29, type='heat') + opts(title="How often do you read these materials because you want to?")

l29g = likert(items29, grouping=pisana$CNT)
summary(l29g)

plot(l29g, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")
