library(dplyr)
dat <- read.csv("eval/MentalHealth_2019to2021_required.csv", stringsAsFactors = T) 

# Select columns Manually
ColsManually <- read.csv("eval/columnsToModeling.csv")
cols <- ColsManually$column_name[!ColsManually$Factor == ""] %>% 
  as.character()
Data <- dat[,cols] 
names(Data) <- ColsManually$Factor[!ColsManually$Factor == ""] %>% 
  as.character()
Data$CurrentlyMentalHealthDisorder <- dat$`Do.you..currently..have.a.mental.health.disorder.`
# write.csv(Data, file = "eval/DataForModeling7Variables.csv", row.names = F)

# Explore missing values
# library(naniar)
# vis_miss(dat)

library(randomForest)
set.seed(5566)
ind <- sample(2, nrow(Data), replace = TRUE, prob = c(0.7, 0.3))
train <- Data[ind==1,]
test <- Data[ind==2,]
rf <- randomForest(CurrentlyMentalHealthDisorder ~ ., data=train, na.action = na.omit) 
print(rf)

# Model performance in Testing set
Prediction <- predict(rf, test)
Reference <- test$CurrentlyMentalHealthDisorder
table(Prediction, Reference)

varImpPlot(rf, sort = TRUE)

importance_df <- importance(rf) %>% 
  as.data.frame() %>% 
  cbind(name = names(Data)[-which(names(Data) == "CurrentlyMentalHealthDisorder")]) %>% 
  arrange(desc(MeanDecreaseGini)) 
importance_df$MeanDecreaseGini <- round(importance_df$MeanDecreaseGini, digits = 2)
write.csv(importance_df, file = "eval/importance_df.csv", row.names = F)  

# Importance top 5
importance_df$name[1:5]
