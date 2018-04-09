#### 1) load packages ####
install.packages("party")
install.packages("caTools")
install.packages("pROC")
library(party)
library(pROC)
library(caTools)


#### 2) load/prep data ####
data <- gb_data
#### 3) create training and test subsets for model ####
data$split <- sample.split(data$cluster, SplitRatio = .7)
gbm_train <- subset(data, data$split == TRUE)
gbm_train <- gbm_train[, -20]
gbm_test <- subset(data, data$split == FALSE)
gbm_test <- gbm_test[, -20]
#### 4) create model ####
ctree1 <- ctree(cluster ~ ., data = gbm_train)
ctree1
#### 5) test models performance ####
ctree_preds <- predict(ctree1, type = "response", newdata = gbm_test)
ctree_prediction <- prediction(ctree_preds, gbm_test$cluster)

roc_ctree <- roc(gbm_test$cluster, ctree_preds)
auc(roc_ctree)

ctree_performance <- performance(ctree_prediction, "tpr", "fpr")
ctree_performance

