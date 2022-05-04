require(likert)

mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')

# Create a dummy data frame. Note that "Item 1" has only four levels
items <- data.frame('Item 1'=factor(sample(mylevels[1:4], 100, replace=TRUE)),
					'Item 2'=factor(sample(mylevels[1:5], 100, replace=TRUE)),
					'Item 3'=factor(sample(mylevels[1:5], 100, replace=TRUE)),
					check.names=FALSE)
str(items)
groups <- sample(c('g1','g2'), 100, replace=TRUE)

tryCatch({
	# This will throw an error because all the items must have the same number of levels.
	lbad <- likert(items)
}, error=function(e) { 
	print("This is good that an error was thrown!")
	print(e) 
})

sapply(items, class) #Verify that all the columns are indeed factors
sapply(items, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(items)) {
	items[,i] <- factor(items[,i], levels=mylevels)
}
lgood <- likert(items)
lgood
summary(lgood)
plot(lgood)

lgr <- likert(items, grouping=groups)
summary(lgr)
plot(lgr)

# If level doesn't appear in any item, does it show in the legend?
items2 <- data.frame('Item 1'=factor(sample(mylevels[1:4], 100, replace=TRUE)),
					'Item 2'=factor(sample(mylevels[1:4], 100, replace=TRUE)),
					'Item 3'=factor(sample(mylevels[1:4], 100, replace=TRUE)),
					check.names=FALSE)
for(i in seq_along(items2)) {
	items2[,i] <- factor(items2[,i], levels=mylevels)
}
str(items2)
l2 <- likert(items2)
plot(l2) # YES!
