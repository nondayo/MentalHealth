library(dplyr)
library(ggplot2)
# library(ggmosaic)
dat <- read.csv("eval/DataForModeling7Variables.csv", stringsAsFactors = T) 
names(dat)
# [1] "EmploymentType"                "JobScope"                     
# [3] "FamilyHistory"                 "Age"                          
# [5] "Gender"                        "CountryLiveIn"                
# [7] "CountryWorkIn"                 "CurrentlyMentalHealthDisorder"

Chi_func <- function(v1, v2){
  tb <- table(v1, v2)
  chi <- chisq.test(tb, correct = F)
  chi$p.value
}

# EmploymentType
spineplot(factor(dat$EmploymentType)~factor(dat$CurrentlyMentalHealthDisorder), 
          xlab = "CurrentlyMentalHealthDisorder", ylab = "EmploymentType",
          col = palette("Tableau"))
Chi_func(dat$EmploymentType, dat$CurrentlyMentalHealthDisorder)
# [1] 0.5091405

# JobScope
spineplot(factor(dat$JobScope)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))
Chi_func(dat$JobScope, dat$CurrentlyMentalHealthDisorder)
# [1] 0.685625

# FamilyHistory ***
spineplot(factor(dat$FamilyHistory)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))
Chi_func(dat$FamilyHistory, dat$CurrentlyMentalHealthDisorder)
# [1] 7.74382e-30 ***

# Gender
spineplot(factor(dat$Gender)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))
Chi_func(dat$Gender, dat$CurrentlyMentalHealthDisorder)
# [1] 0.3106059

# CountryLiveIn ***
spineplot(factor(dat$CountryLiveIn)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))
Chi_func(dat$CountryLiveIn, dat$CurrentlyMentalHealthDisorder)
# [1] 8.454039e-11 ***

# CountryWorkIn ***
spineplot(factor(dat$CountryWorkIn)~factor(dat$CurrentlyMentalHealthDisorder), col = palette("Tableau"))
Chi_func(dat$CountryWorkIn, dat$CurrentlyMentalHealthDisorder)
# [1] 1.893469e-13 ***

# Age
# boxplot(Age ~ factor(CurrentlyMentalHealthDisorder), data = dat)
ggplot(dat, aes(x = CurrentlyMentalHealthDisorder, y = Age)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

