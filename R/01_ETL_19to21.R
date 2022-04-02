library(dplyr)
dat2019 <- read.csv("data/OSMI 2019 Mental Health in Tech Survey Results - OSMI Mental Health in Tech Survey 2019.csv", stringsAsFactors = T)
dat2020 <- read.csv("data/OSMI 2020 Mental Health in Tech Survey Results .csv", stringsAsFactors = T)
dat2021 <- read.csv("data/OSMI 2021 Mental Health in Tech Survey Results .csv", stringsAsFactors = T)

# Deal with categorical columns
cols_in_common <- names(dat2019)[names(dat2019) %in% names(dat2020)]
# check
# any(!cols_in_common %in% names(dat2021))
# [1] FALSE

cols_text <- c(grep(cols_in_common, pattern = "Describe", value = T),
               grep(cols_in_common, pattern = "Why.or.why.not.", value = T),
               grep(cols_in_common, pattern = "describe", value = T),
               grep(cols_in_common, pattern = "If.there.is.anything.else.you.would.like.to.tell.us.", value = T),
               grep(cols_in_common, pattern = "Would.you.be.willing.to.talk.to.one.of.us.more.extensively.about.your.experiences.with.mental.health.issues.", value = T)
)

cols_adjust_levels <- sapply(cols_in_common, function(col){
  if(is.factor(dat2019[[col]])){
    if(!all(levels(dat2019[[col]]) == levels(dat2020[[col]]))){
      col
    }
  }
}) %>% 
  unlist()
cols_adjust_levels <- cols_adjust_levels[!cols_adjust_levels %in% cols_text]

for(i in 1:length(cols_adjust_levels)){
  all_levels <- c(levels(dat2019[[cols_adjust_levels[i]]]), levels(dat2020[[cols_adjust_levels[i]]])) %>% unique()
  dat2019[[cols_adjust_levels[i]]] <- factor(dat2019[[cols_adjust_levels[i]]], levels = all_levels)
  dat2020[[cols_adjust_levels[i]]] <- factor(dat2020[[cols_adjust_levels[i]]], levels = all_levels)
  dat2021[[cols_adjust_levels[i]]] <- factor(dat2021[[cols_adjust_levels[i]]], levels = all_levels)
}

# Check
sapply(cols_adjust_levels, function(col){
  if(is.factor(dat2019[[col]])){
    if(!all(levels(dat2019[[col]]) == levels(dat2020[[col]]))){
      col
    }else{
      "Levels all the same!"
    }
  }
}) %>% unique()

# Extract columns and merge
final_cols <- cols_in_common[!cols_in_common %in% cols_text]
d1 <- select(dat2019, final_cols) %>% 
  mutate(year = "2019")
d2 <- select(dat2020, final_cols) %>% 
  mutate(year = "2020")
d3 <- select(dat2021, final_cols) %>% 
  mutate(year = "2021")
dat <- rbind(d1, d2) %>% 
  rbind(d3)

# Rename the levels of categorical columns
cols_0_1 <- c("X.Are.you.self.employed..", "Is.your.employer.primarily.a.tech.company.organization.",
              "Is.your.primary.role.within.your.company.related.to.tech.IT.",
              "Have.you.ever.discussed.your.mental.health.with.your.employer.",
              "Have.you.ever.discussed.your.mental.health.with.coworkers.",
              "Have.you.ever.had.a.coworker.discuss.their.or.another.coworker.s.mental.health.with.you.",
              "Do.you.have.medical.coverage..private.insurance.or.state.provided..that.includes.treatment.of.mental.health.disorders.",
              "X.Do.you.have.previous.employers..",
              "Was.your.employer.primarily.a.tech.company.organization.",
              "Did.you.ever.discuss.your.mental.health.with.your.previous.employer.",
              "Did.you.ever.discuss.your.mental.health.with.a.previous.coworker.s..",
              "Did.you.ever.have.a.previous.coworker.discuss.their.or.another.coworker.s.mental.health.with.you.",
              "Have.you.ever.sought.treatment.for.a.mental.health.disorder.from.a.mental.health.professional.",
              "Are.you.openly.identified.at.work.as.a.person.with.a.mental.health.issue.",
              "Has.being.identified.as.a.person.with.a.mental.health.issue.affected.your.career.")
for(col in cols_0_1){
  dat[[col]] <- dat[[col]] %>%
    as.character() %>%
    factor(levels = c("1", "0"), labels = c("Yes", "No"))
}

# Deal with NA
for(col in names(dat)){
  dat[[col]][dat[[col]] == ""] <- NA
  dat[[col]][dat[[col]] == "N/A"] <- NA
}
write.csv(dat, file = "eval/MentalHealth_2019to2021.csv", row.names = F)

dat_required <- dat[, c(1:18, 42, 44:53, 60:67)]
write.csv(dat_required, file = "eval/MentalHealth_2019to2021_required.csv", row.names = F)
