library(likert)
data("pisaitems")

items24 <- pisaitems[sample(nrow(pisaitems), 200),# Sampling so that the CI is wide enough to see
					 substr(names(pisaitems), 1,5) == 'ST24Q']
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

tab <- summary(l24)
tab$se <- sqrt( (tab$low * tab$high) / nrow(items24) )
cv <- qnorm(0.05 / 2, lower.tail = FALSE) # Critical value, here 95% CI
# Use this if not centered on zero
# tab$ci.low <- tab$low - cv * tab$se
# tab$ci.high <- tab$high + cv * tab$se
tab$ci.low <- - cv * tab$se
tab$ci.high <- cv * tab$se

plot(l24) + geom_errorbar(data = tab, aes(ymin = ci.low, ymax = ci.high, y = low))

# Not centered on zero
tab <- summary(l24)
tab$se <- sqrt( (tab$low * tab$high) / nrow(items24) )
cv <- qnorm(0.05 / 2, lower.tail = FALSE) # Critical value, here 95% CI
tab$ci.low <-  tab$low - cv * tab$se
tab$ci.high <- tab$low + cv * tab$se

plot(l24, centered = FALSE) + geom_errorbar(data = tab, aes(ymin = ci.low, ymax = ci.high, y = low))

