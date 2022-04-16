library(dplyr)
library(ggplot2)
# library(ggmosaic)
dat <- read.csv("eval/DataForModeling8Variables.csv", stringsAsFactors = T) 

Chi_func <- function(v1, v2){
  tb <- table(v1, v2)
  chi <- chisq.test(tb, correct = F)
  chi$p.value
}

# EmploymentType
spineplot(factor(dat$EmploymentType)~factor(dat$CurrentlyMentalHealthDisorder), 
          xlab = "CurrentlyMentalHealthDisorder", ylab = "EmploymentType",
          col = palette("Tableau"))

# JobScope
spineplot(factor(dat$JobScope)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))

# FamilyHistory 
spineplot(factor(dat$FamilyHistory)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))

# Gender
spineplot(factor(dat$Gender)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))

# CountryLiveIn 
spineplot(factor(dat$CountryLiveIn)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))

# CountryWorkIn 
spineplot(factor(dat$CountryWorkIn)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))

# Age
# boxplot(Age ~ factor(CurrentlyMentalHealthDisorder), data = dat)
ggplot(dat, aes(x = CurrentlyMentalHealthDisorder, y = Age)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

