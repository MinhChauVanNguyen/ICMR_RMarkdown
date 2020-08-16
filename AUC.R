library(caret)
library(randomForest)
library(ROCR)
library(pROC)
library(MLeval)


dat <- data[names(data) %in% c("GROUP", "Cause_of_redo")]

dat$GROUP <- factor(dat$GROUP)
dat$Cause_of_redo <- factor(dat$Cause_of_redo)

set.seed(2262)
train_ind <- sample(seq_len(nrow(dat)), size = floor(0.80 * nrow(dat)))
train <- dat[train_ind, ]
test <- dat[-train_ind, ]

########

fitRF <- randomForest(formula = Cause_of_redo~GROUP, data = train)

predictions <- as.data.frame(predict(fitRF, test, type = "prob"))

predictions$predict <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
predictions$observed <- test$Cause_of_redo

roc.MR <- roc(ifelse(predictions$observed=="MR", "MR", "non-MR"), as.numeric(predictions$MR))
roc.MS <- roc(ifelse(predictions$observed=="MS", "MS", "non-MS"), as.numeric(predictions$MR))
roc.NONE <- roc(ifelse(predictions$observed=="NONE", "NONE", "others"), as.numeric(predictions$MR))
predictions$predict <- as.ordered(predictions$predict)
auc <- multiclass.roc(test$Cause_of_redo, predictions)
plot(roc.MR, col = "gray60")
lines(roc.MS, col = "blue")
lines(roc.MR, col = "red")
text(0.9,0.5, paste("AUC = ", format(auc$auc, digits=5, scientific=FALSE)))



####
fitRF <- randomForest(Cause_of_redo~GROUP, train)

predictions <- as.data.frame(predict(fitRF, test, type = "prob"))
predictions$predict <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
predictions$predict <- as.ordered(as.numeric(predictions$predict))
auc <- multiclass.roc(test$Cause_of_redo, predictions$predict)

rs <- auc[['rocs']]
plot.roc(rs[[1]], xlab = "1 - Specificity", xlim = c(1:0))
axis(1, at = 1:0)
text(0.9,0.5, paste("AUC = ", format(auc$auc, digits=5, scientific=FALSE)))
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))


#####
predicted <- predict(fitRF, test)
confusionMatrix(predicted, test$Cause_of_redo) 

confusion_table <- table(predicted, test$Cause_of_redo)

(accuracy <- sum(diag(confusion_table)) / sum(confusion_table))
(precision <- diag(confusion_table) / rowSums(confusion_table))
(recall <- diag(confusion_table) / colSums(confusion_table))
