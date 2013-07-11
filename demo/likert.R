options(digits=2)
theme_update(panel.background=element_rect(size=1, color='grey70', fill=NA) )

require(likert)
data(pisaitems)

##### Item 28: Reading Attitudes
items28 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
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
str(items28)

l28 = likert(items28)
l28 #print(l28)
summary(l28)
summary(l28, center=1.5)
summary(l28, center=2)

plot(l28)
plot(l28, centered=TRUE, wrap=30)
plot(l28, centered=TRUE, center=1.5, wrap=30)
plot(l28, centered=TRUE, center=2, wrap=30)
plot(l28, centered=TRUE, center=2, include.center=FALSE, wrap=30)

plot(l28, type='density')
plot(l28, type='density', facet=FALSE)

plot(l28, type='heat', wrap=30)

##### Group by Country
l28g <- likert(items28, grouping=pisaitems$CNT)
print(l28g)
summary(l28g)
summary(l28g, center=1.5)
summary(l28g, center=2)

plot(l28g)
plot(l28g, centered=TRUE)
plot(l28g, centered=TRUE, center=1.5)
plot(l28g, centered=TRUE, center=2)
plot(l28g, centered=TRUE, center=2, include.center=FALSE)

plot(l28g, type='density')

##### Item 29: How often do you read these materials because you want to?
title <- "How often do you read these materials because you want to?"
items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")

l29 <- likert(items29)
print(l29)
summary(l29)

plot(l29) + ggtitle(title)
plot(l29, centered=TRUE) + ggtitle(title)
plot(l29, centered=TRUE, include.center=FALSE) + ggtitle(title)
plot(l29, centered=TRUE, center=2) + ggtitle(title)
plot(l29, centered=TRUE, center=2.5) + ggtitle(title)

plot(l29, type='density') + ggtitle(title)
plot(l29, type='density', facet=FALSE, legend='Material') + ggtitle(title)

plot(l29, type='heat') + ggtitle(title)

##### Grouped by country
l29g <- likert(items29, grouping=pisaitems$CNT)
summary(l29g)

plot(l29g) + ggtitle(title)
plot(l29g, centered=TRUE) + ggtitle(title)
plot(l29g, centered=TRUE, include.center=FALSE) + ggtitle(title)
plot(l29g, centered=TRUE, center=2)
plot(l29g, centered=TRUE, center=2.5) + ggtitle(title)

plot(l29g, type='density', legend='Country') + ggtitle(title)
