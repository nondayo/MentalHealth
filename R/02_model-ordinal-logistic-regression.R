# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
library(dplyr)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
dat <- read.csv("eval/DataForModeling7Variables.csv", stringsAsFactors = T) 
# anyNA(dat)
# names(dat)
# [1] "EmploymentType"                "JobScope"                     
# [3] "FamilyHistory"                 "Age"                          
# [5] "Gender"                        "CountryLiveIn"                
# [7] "CountryWorkIn"                 "CurrentlyMentalHealthDisorder"

# Explore missing values
# library(naniar)
# vis_miss(dat)

## fit ordered logit model and store results 'm'
# Reference
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
m <- polr(CurrentlyMentalHealthDisorder ~ ., data = dat, Hess=TRUE)
## view a summary of the model
summary(m)
# Call:
#   polr(formula = CurrentlyMentalHealthDisorder ~ ., data = dat, 
#        Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error  t value
# JobScopeYes                           -0.61781    0.36436 -1.69562
# FamilyHistoryNo                       -1.27548    0.28887 -4.41546
# FamilyHistoryYes                       0.47310    0.26328  1.79693
# Age                                    0.01136    0.01127  1.00803
# Gendermale                             0.24359    0.40160  0.60655
# Genderother                            1.34032    0.91124  1.47087
# CountryLiveInUnited Kingdom            0.53618    0.44124  1.21516
# CountryLiveInUnited States of America  0.99465    1.71894  0.57864
# CountryWorkInother                    -0.70128    1.74865 -0.40104
# CountryWorkInUnited States of America  0.05121    1.72299  0.02972
# 
# Intercepts:
#               Value   Std. Error t value
# Don't Know|No -2.5145  0.6847    -3.6726
# No|Possibly   -0.0018  0.6617    -0.0028
# Possibly|Yes   1.1756  0.6621     1.7755
# 
# Residual Deviance: 812.7212 
# AIC: 838.7212 
# (291 observations deleted due to missingness)

(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
#                                              Value Std. Error     t value      p value
# JobScopeYes                           -0.617808422 0.36435514 -1.69562154 8.995758e-02
# FamilyHistoryNo                       -1.275476730 0.28886636 -4.41545617 1.007972e-05 ***
# FamilyHistoryYes                       0.473097035 0.26328096  1.79692843 7.234698e-02
# Age                                    0.011359879 0.01126939  1.00802981 3.134402e-01
# Gendermale                             0.243592697 0.40160370  0.60654993 5.441496e-01
# Genderother                            1.340322325 0.91124328  1.47087211 1.413257e-01
# CountryLiveInUnited Kingdom            0.536180962 0.44124448  1.21515619 2.243065e-01
# CountryLiveInUnited States of America  0.994645649 1.71894466  0.57863739 5.628339e-01
# CountryWorkInother                    -0.701277738 1.74864933 -0.40103966 6.883909e-01
# CountryWorkInUnited States of America  0.051205285 1.72298969  0.02971886 9.762913e-01
# Don't Know|No                         -2.514494311 0.68466873 -3.67257066 2.401227e-04 
# No|Possibly                           -0.001833816 0.66165733 -0.00277155 9.977886e-01
# Possibly|Yes                           1.175585206 0.66209858  1.77554406 7.580810e-02