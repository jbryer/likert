# Using pre-summarized data and no groups
data(MathAnxiety)

lmass <- likert(summary=MathAnxiety)
print(lmass)
summary(lmass)
plot(lmass)

# Using pre-summarized data and grouped by gender
data(MathAnxietyGender)
lgmass <- likert(summary=MathAnxietyGender[,2:ncol(MathAnxietyGender)], 
				 grouping=MathAnxietyGender$Group)
print(lgmass)
summary(lgmass)
plot(lgmass)

##### Verify the above using the original raw data.
# No groups
data(mass)
lmass.orig <- likert(mass[,2:ncol(mass)])
print(lmass.orig)
summary(lmass.orig)
plot(lmass.orig)

# Grouped by gender
lgmass.orig <- likert(mass[,2:ncol(mass)], grouping=mass$Gender)
print(lgmass.orig)
summary(lgmass.orig)
plot(lgmass.orig)
