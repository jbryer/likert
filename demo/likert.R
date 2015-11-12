options(digits=2)

require(likert)
data(pisaitems)

##### Item 24: Reading Attitudes
items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
head(items24); ncol(items24)

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
str(items24)

l24 <- likert(items24)
l24 #print(l24)
summary(l24)
summary(l24, center=1.5)
summary(l24, center=2)

# xtable
xtable(l24)

# Plots
plot(l24)
plot(l24, ordered=FALSE, group.order=names(items24)) #Specify the exact order of the y-axis
plot(l24, centered=FALSE, wrap=30)
plot(l24, center=1.5, wrap=30)
plot(l24, center=2, wrap=30)
plot(l24, center=2, include.center=FALSE, wrap=30)
plot(l24, center=2, include.center=FALSE, wrap=20)
plot(l24, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)

plot(l24, colors=c('orange','darkorange','darkblue','blue'))

#Include histogram with response counts
plot(l24, include.histogram=TRUE)

# Density plot
plot(l24, type='density')
plot(l24, type='density', facet=FALSE)

# Heat map
plot(l24, type='heat', wrap=30, text.size=4)

# Reverse the levels
items24.reverse <- reverse.levels(items24)
l24.reverse <- likert(items24.reverse)
print(l24.reverse)
plot(l24.reverse)


##### Group by Country
l24g <- likert(items24, grouping=pisaitems$CNT)
print(l24g)
summary(l24g)
summary(l24g, center=1.5)
summary(l24g, center=2)

# xtable
# xtable(l24g) # TODO: Doesn't work!

# Plots
plot(l24g)
plot(l24g, centered=FALSE)
plot(l24g, center=1.5)
plot(l24g, center=2)
plot(l24g, center=2, include.center=FALSE)
plot(l24g, group.order=c('Mexico', 'Canada', 'United States'))

#Include histogram with response counts
plot(l24g, include.histogram=TRUE)
plot(l24g, include.histogram=TRUE, group.order=c('Mexico', 'Canada', 'United States'))

# Alternate panel arrangements.
plot(l24g, panel.arrange='h', wrap=20)
plot(l24g, panel.arrange=NULL, wrap=40)

# Density plots
plot(l24g, type='density')

# Reordering the groups
plot(l24g, group.order=c('Canada', 'Mexico', 'United States'))


##### Item 29: How often do you read these materials because you want to?
title <- "How often do you read these materials because you want to?"
items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")

l29 <- likert(items29)
print(l29)
summary(l29)

# xtable
xtable(l29)

# Plots
plot(l29) + ggtitle(title)
plot(l29, centered=TRUE) + ggtitle(title)
plot(l29, centered=TRUE, include.center=FALSE) + ggtitle(title)
plot(l29, centered=TRUE, center=2) + ggtitle(title)
plot(l29, centered=TRUE, center=2.5) + ggtitle(title)
# Turn off neutral labels
plot(l29, plot.percent.neutral=FALSE) + ggtitle(title)


# Density plots
plot(l29, type='density') + ggtitle(title)
plot(l29, type='density', facet=FALSE, legend='Material') + ggtitle(title)

# Heat maps
plot(l29, type='heat') + ggtitle(title)

##### Grouped by country
l29g <- likert(items29, grouping=pisaitems$CNT)
summary(l29g)

# Plots
plot(l29g) + ggtitle(title)
plot(l29g, centered=FALSE) + ggtitle(title)
plot(l29g, centered=TRUE, include.center=FALSE) + ggtitle(title)
plot(l29g, centered=TRUE, center=2)
plot(l29g, centered=TRUE, center=2.5) + ggtitle(title)

# Density plots
plot(l29g, type='density', legend='Country') + ggtitle(title)


## With only one varaible
lone <- likert(items24[,1, drop=FALSE])
plot(lone)

loneg <- likert(items24[,1, drop=FALSE], grouping=pisaitems$CNT)
plot(loneg)

