require(likert)
data(gap)

l.sat <- likert(items=gap[,2:6], nlevels=7)
plot(l.sat) + ggtitle('Satisfaction')

l.imp <- likert(items=gap[,7:11], nlevels=7)
plot(l.imp) + ggtitle('Importance')

l.gap <- likert(items=gap[,2:6], importance=gap[,7:11], nlevels=7)
class(l.gap)

summary(l.gap)
print(l.gap, row.names=FALSE)

#plot(l.gap) # Default is bar
plot(l.gap, type='density')
splot(l.gap, type='heat')

