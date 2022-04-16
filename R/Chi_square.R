library(dplyr)
dat <- read.csv("eval/DataForModeling8Variables.csv", stringsAsFactors = T) 

Chi_func <- function(v1, v2){
  tb <- table(v1, v2)
  chi <- chisq.test(tb, correct = F)
  chi$p.value
}

# 01
# FamilyHistory ***
Chi_func(dat$FamilyHistory, dat$CurrentlyMentalHealthDisorder)

# Age
var.test(dat$Age ~ dat$CurrentlyMentalHealthDisorder)
# p-value = 0.1335
# Since p-value > 0.05, the variance of Age in two groups are equal.
# t.test
t.test(dat$Age ~ dat$CurrentlyMentalHealthDisorder, var.equal = TRUE)
# p-value = 0.0621
# Since p-value > 0.05, we can accept the null hypothesis.
# In other words, there are no difference of age between the two groups of mental health disorders situation.

# CountryWorkIn ***
Chi_func(dat$CountryWorkIn, dat$CurrentlyMentalHealthDisorder)

# CountryLiveIn ***
Chi_func(dat$CountryLiveIn, dat$CurrentlyMentalHealthDisorder)

# Gender
Chi_func(dat$Gender, dat$CurrentlyMentalHealthDisorder)

