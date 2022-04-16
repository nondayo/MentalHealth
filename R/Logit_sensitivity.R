## fit logistic model and store results 'logistic_model'
logistic_model <- glm(as.factor(CurrentlyMentalHealthDisorder) ~ ., 
                      data = dat, family = "binomial")
## view a summary of the model
summary(logistic_model)

par(pty="s", mfrow=c(1, 1))

cutoff <- seq(0.1, 0.9, 0.1)
v.acc <- vector()
v.sen <- vector()
for (i in cutoff) {
  pred <- as.factor(ifelse(logistic_model$fitted.values >= i, "Yes", "Other"))
  pred <- factor(pred, levels=c("Yes", "Other"))
  ref <- as.factor(dat$CurrentlyMentalHealthDisorder)
  ref <- factor(ref, levels=c("Yes", "Other"))
  cm <- confusionMatrix(pred, ref)
  acc <- cm$overall[1]
  v.acc <- append(v.acc, acc)
  sen <- cm$byClass["Sensitivity"]
  v.sen <- append(v.sen, sen)
}
names(v.acc) <- cutoff
barplot(v.acc, main="Accuracy by threshold",
        xlab="Threshold")
  
names(v.sen) <- cutoff
barplot(v.sen, main="Sensitivity by threshold",
        xlab="Threshold")
  
pred <- as.factor(ifelse(logistic_model$fitted.values >= 0.1, "Yes", "Other"))
pred <- factor(pred, levels=c("Yes", "Other"))
ref <- as.factor(dat$CurrentlyMentalHealthDisorder)
ref <- factor(ref, levels=c("Yes", "Other"))
cm <- confusionMatrix(pred, ref)
cm
