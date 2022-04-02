library(dplyr)
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T) %>%
  select(-c("What.US.state.or.territory.do.you..live..in.",
            "What.is.your.race.",
            "What.US.state.or.territory.do.you..work..in.",
            "If.you.have.a.mental.health.disorder..how.often.do.you.feel.that.it.interferes.with.your.work..when.being.treated.effectively..",
            "If.you.have.a.mental.health.disorder..how.often.do.you.feel.that.it.interferes.with.your.work..when.._.NOT._..being.treated.effectively..i.e...when.you.are.experiencing.symptoms...",
            "year")) #%>% 
  # na.omit()



dat$`Do.you..currently..have.a.mental.health.disorder.` %>% table
# Don't Know         No   Possibly        Yes 
#         64        212        146        241 


which(names(dat) == "Do.you..currently..have.a.mental.health.disorder.")
# 19
original_names <- names(dat)
names(dat) <- sapply(1:length(dat), function(i){paste0("V", i)})
# names(dat)

# Explore missing values
# library(naniar)
# vis_miss(dat)

library(randomForest)
set.seed(5566)
ind <- sample(2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))
train <- dat[ind==1,]
test <- dat[ind==2,]
# rf <- randomForest(V19 ~ ., data=train) 
rf <- randomForest(V19 ~ ., data=train, na.action = na.omit) 
print(rf)
# Confusion matrix:
#   Don't Know No Possibly Yes class.error
# Don't Know          0 13       10   2   1.0000000
# No                  0 47       12  13   0.3472222
# Possibly            0 26       13  18   0.7719298
# Yes                 0  7        6  93   0.1226415
Prediction <- predict(rf, train)
Reference <- train$V19
table(Prediction, Reference)


varImpPlot(rf, sort = TRUE)

importance_df <- importance(rf) %>% 
  as.data.frame() %>% 
  cbind(name = original_names[-19]) %>% 
  arrange(desc(MeanDecreaseGini)) #%>% 
  # View()
importance_df$MeanDecreaseGini <- round(importance_df$MeanDecreaseGini, digits = 2)
write.csv(importance_df, file = "eval/importance_df.csv", row.names = F)  

# Importance top 5
importance_df$name[1:5]
