#### Load Packages ####
install.packages("gbm")
install.packages("caTools")
install.packages("pROC")
library(pROC)
library(caTools)
library(gbm)
#### Create binary cluster variable ####
data <- variables
data$cluster <- ifelse(data$cluster == 2, 1, 0)

#### create training and test sets for data ####
data$split <- sample.split(data$cluster, SplitRatio = .7)
gbm_train <- subset(data, data$split == TRUE)
gbm_train <- gbm_train[, -20]
gbm_test <- subset(data, data$split == FALSE)
gbm_test <- gbm_test[, -20]
#### create gbm model ####
gbm_model <- gbm(formula = cluster ~ .,
                 distribution = "bernoulli",
                 data = gbm_train,
                 n.trees = 10000)
summary(gbm_model)

gbm_preds <- predict(object = gbm_model,
        newdata = gbm_test,
        n.trees = 10000,
        type = "response")
#### Test accuracy (AUC) ####
roc_gbm <- roc(gbm_test$cluster, gbm_preds)
auc(roc_gbm)


