dat <- read.csv("eval/DataForModeling8Variables.csv", stringsAsFactors = T) 
dim(dat)
library(randomForest)

set.seed(5)
# default
rf <- randomForest(CurrentlyMentalHealthDisorder ~ ., 
                   data=dat,
                   na.action = na.omit) 
print(rf)
# importance(rf)

# number of variables at each split
rf1.tune <- tuneRF(dat[, -which(names(dat) == "CurrentlyMentalHealthDisorder")], dat$CurrentlyMentalHealthDisorder, doBest=T) 
plot(rf1.tune)
rf1.tune

model1 <- randomForest(CurrentlyMentalHealthDisorder ~ ., 
                       dat, ntree = 2000)
print(model1)
oob.err.data1 <- data.frame(
  Trees = rep(1:nrow(model1$err.rate), 3), 
  Type = rep(c("OOB","Other","Yes"), each = nrow(model1$err.rate)),
  Error = c(model1$err.rate[,"OOB"], model1$err.rate[,"Other"], model1$err.rate[,"Yes"]))
library(ggplot2)
ggplot(data = oob.err.data1, aes(x = Trees, y= Error)) + 
  geom_line(aes(color = Type))

rf2 <- randomForest(CurrentlyMentalHealthDisorder ~ ., 
                   data=dat,
                   na.action = na.omit,
                   mtry=2) # explicitly specified
print(rf2)
importance(rf2)

# number of trees to grow 
rf3 <-  randomForest(CurrentlyMentalHealthDisorder ~ ., 
                     data=dat,
                     na.action = na.omit,
                     mtry=2,
                     ntree=2000) 
print(rf3)
importance(rf3)
plot(rf3)

