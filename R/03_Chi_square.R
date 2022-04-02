# 02
library(dplyr)
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T)
# levels(dat$`Do.you..currently..have.a.mental.health.disorder.`)
# [1] "Don't Know" "No"         "Possibly"   "Yes"  
tb <- table(dat$`Do.you..currently..have.a.mental.health.disorder.`, 
                dat$`Do.you.have.a.family.history.of.mental.illness.`)
chi <- chisq.test(tb, correct = F)
chi$p.value
# > chi$p.value
# [1] 5.265167e-30

# 03
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T) 
dat$disorder <- ifelse(dat$`Do.you..currently..have.a.mental.health.disorder.` == "Yes", "Yes", "No")
dat$Pandemic <- ifelse(dat$year < 2020, "Before", "After")
tb <- table(dat$disorder, 
            dat$Pandemic)
chi <- chisq.test(tb, correct = F)
chi$p.value
# > chi$p.value
# [1] 0.002057285


# 04-1 DiscussWithSupervisor
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T)
dat$DiscussWithSupervisor <- dat$`Would.you.feel.comfortable.discussing.a.mental.health.issue.with.your.direct.supervisor.s..`
dat$Pandemic <- ifelse(dat$year < 2020, "Before", "After")
tb <- table(dat$DiscussWithSupervisor, 
            dat$Pandemic)
chi <- chisq.test(tb, correct = F)
chi$p.value
# > chi$p.value
# [1] 0.01745957

# 04-2 DiscussWithCoworkers
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T)
dat$DiscussWithCoworkers <- dat$`Would.you.feel.comfortable.discussing.a.mental.health.issue.with.your.coworkers.`
dat$Pandemic <- ifelse(dat$year < 2020, "Before", "After")
tb <- table(dat$DiscussWithCoworkers,
            dat$Pandemic)
chi <- chisq.test(tb, correct = F)
chi$p.value
# > chi$p.value
# [1] 0.3099547