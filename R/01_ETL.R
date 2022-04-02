dat2014 <- read.csv("data/survey2014.csv")
dat2016 <- read.csv("data/mental-heath-in-tech-2016_20161114.csv")

# Adjust column names
names(dat2014) %in% names(dat2016)

names(dat2016)[which(names(dat2016) == "")] <- ""
names(dat2016)[which(names(dat2016) == "What.is.your.age.")] <- "Age"
names(dat2016)[which(names(dat2016) == "What.is.your.gender.")] <- "Gender"
names(dat2016)[which(names(dat2016) == "What.country.do.you.work.in.")] <- "Country"
names(dat2016)[which(names(dat2016) == "What.US.state.or.territory.do.you.live.in.")] <- "state"
names(dat2016)[which(names(dat2016) == "Are.you.self.employed.")] <- "self_employed"
names(dat2016)[which(names(dat2016) == "Do.you.have.a.family.history.of.mental.illness.")] <- "family_history"
# 2014 Survey: treatment: Have you sought treatment for a mental health condition?
# 2016 Survey: 
# grep(names(dat2016), pattern = "treatment", value = T)
# [3] "Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer."
# [2] "Do.you.have.medical.coverage..private.insurance.or.state.provided..which.includes.treatment.of..mental.health.issues."                         
# [3] "Was.your.anonymity.protected.if.you.chose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.with.previous.employers."  
# [4] "Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional."  
names(dat2016)[which(names(dat2016) == "Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional.")] <- "treatment"
# 2014 Survey: work_interfere: If you have a mental health condition, do you feel that it interferes with your work?
# 2016 Survey:
# If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.being.treated.effectively.
# If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.NOT.being.treated.effectively.
# names(dat2016)[which(names(dat2016) == "")] <- "work_interfere"
names(dat2016)[which(names(dat2016) == "How.many.employees.does.your.company.or.organization.have.")] <- "no_employees"
names(dat2016)[which(names(dat2016) == "Do.you.work.remotely.")] <- "remote_work"
names(dat2016)[which(names(dat2016) == "Is.your.employer.primarily.a.tech.company.organization.")] <- "tech_company"
names(dat2016)[which(names(dat2016) == "Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.")] <- "benefits"
names(dat2016)[which(names(dat2016) == "")] <- "care_options"
names(dat2016)[which(names(dat2016) == "")] <- "wellness_program"
names(dat2016)[which(names(dat2016) == "")] <- "seek_help"
names(dat2016)[which(names(dat2016) == "")] <- "anonymity"
names(dat2016)[which(names(dat2016) == "")] <- "leave"
names(dat2016)[which(names(dat2016) == "")] <- "mental_health_consequence"
names(dat2016)[which(names(dat2016) == "")] <- "phys_health_consequence"
names(dat2016)[which(names(dat2016) == "")] <- "coworkers"
names(dat2016)[which(names(dat2016) == "")] <- "supervisor"
names(dat2016)[which(names(dat2016) == "")] <- "mental_health_interview"
names(dat2016)[which(names(dat2016) == "")] <- "phys_health_interview"
names(dat2016)[which(names(dat2016) == "")] <- "mental_vs_physical"
names(dat2016)[which(names(dat2016) == "")] <- "obs_consequence"
names(dat2016)[which(names(dat2016) == "")] <- "comments"



grep(names(dat2016), pattern = "treatment", value = T)





# Dealing with factors