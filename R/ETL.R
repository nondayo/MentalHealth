library(dplyr)
dat2019 <- read.csv("data/OSMI 2019 Mental Health in Tech Survey Results - OSMI Mental Health in Tech Survey 2019.csv", stringsAsFactors = T)
dat2020 <- read.csv("data/OSMI 2020 Mental Health in Tech Survey Results .csv", stringsAsFactors = T)
dat2021 <- read.csv("data/OSMI 2021 Mental Health in Tech Survey Results .csv", stringsAsFactors = T)

# Data For Modeling (7 Variables)
# Select columns Manually
selected_columns <- c("Do.you..currently..have.a.mental.health.disorder.",
                      "X.Are.you.self.employed..",
                      "Is.your.primary.role.within.your.company.related.to.tech.IT.",
                      "Do.you.have.a.family.history.of.mental.illness.",
                      "What.is.your.age.",
                      "What.is.your.gender.",
                      "What.country.do.you..live..in.",
                      "What.country.do.you..work..in.")
dat2019 <- dat2019 %>% select(selected_columns) %>% mutate(year = "2019")
dat2020 <- dat2020 %>% select(selected_columns) %>% mutate(year = "2020")
dat2021 <- dat2021 %>% select(selected_columns) %>% mutate(year = "2021")

dat <- rbind(dat2019, dat2020) %>% 
  rbind(dat2021)
names(dat) <- c("CurrentlyMentalHealthDisorder",
                "EmploymentType", 
                "JobScope",
                "FamilyHistory",
                "Age",
                "Gender",
                "CountryLiveIn",
                "CountryWorkIn",
                "year")

# Deal with gender
dat$Gender <- tolower(dat$Gender)
dat$Gender[grepl(dat$Gender, pattern = "male")] <- "male"
dat$Gender[grepl(dat$Gender, pattern = "woman")] <- "female"
dat$Gender[grepl(dat$Gender, pattern = "man")] <- "female"
dat$Gender[dat$Gender == "m"] <- "male"
dat$Gender[dat$Gender == "f"] <- "female"
dat$Gender[!(dat$Gender == "male" | dat$Gender == "female")] <- "other"
# check
# table(dat$Gender)

# Deal with CountryLiveIn
dat$CountryLiveIn <- dat$CountryLiveIn %>% as.character()
dat$CountryLiveIn[!(dat$CountryLiveIn == "United States of America" | 
                      dat$CountryLiveIn == "India" | 
                      dat$CountryLiveIn == "United Kingdom")] <- "other"
# Deal with CountryWorkIn
dat$CountryWorkIn <- dat$CountryWorkIn %>% as.character()
dat$CountryWorkIn[!(dat$CountryWorkIn == "United States of America" | 
                      dat$CountryWorkIn == "India" | 
                      dat$CountryWorkIn == "United Kingdom")] <- "other"
# Remove weird Age
dat <- dat %>% 
  filter(Age < 100 | Age > 18)
write.csv(dat, file = "eval/DataForModeling9Variables.csv", row.names = F)
dat$CurrentlyMentalHealthDisorder <- ifelse(dat$CurrentlyMentalHealthDisorder == "Yes", "Yes", "Other")
write.csv(dat, file = "eval/DataForModeling9Variables2levels.csv", row.names = F)
