library(likert)
data(mass)

str(mass)

lmass <- likert(mass[,-1])
summary(lmass)
plot(lmass)
