
#### Step 1: Load packages####
library(caTools)
library(randomForest)
library(ROCR)
library(party)
library(picante)
library(rpart)
library(rpart.plot)
library(tidyverse)
#### Step 2: Load and Prep Data####
gb_data <- df
#### Step 3: Set clustering variables ####
gb_data <- gb_data[rowSums(dplyr::select(gb_data, NonInvShru, 
                                         InvAnnGras,
                                         InvAnnFo_1,
                                         NonInvPere,
                                         SagebrushC,
                                         BareSoilCo
                                         )) > 0,]


gb_data_clst <- dplyr::select(gb_data, NonInvShru, 
                                       InvAnnGras,
                                       InvAnnFo_1,
                                       NonInvPere,
                                       BareSoilCo
)
gb_data$pca1 <- prcomp(gb_data_clst)$x[,1]

# setwd("~/fire_proj/wet_dry" )
# gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)
# gb_data <- gb_data[rowSums(gb_data[,c(31, 36)]) > 0,]
# gb_data_clst <- gb_data[,c(31, 38)]

#### step 4 Finding Clusters for Veg. cover ####

comm <- decostand(gb_data_clst, method = "total")
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)

# add cluster variable to gb_data
gb_data$cluster <- as.factor(cutree(comm.bc.clust, 2))


gb_data$binary <- as.factor(ifelse(gb_data$NonInvShru < 5, "Grass", "Shrub"))


#### step 5: Determine proper parameters for ntree and mtry ####

## ntree

#### for loop for testing number of trees
# ntree = 2500
#  for (i in 1:10) {
#     forest_1 <- randomForest(cluster ~ sr_band1 +sr_band2 +sr_band3 +sr_band4 +sr_band5 + sr_band7 + NDVI + EVI + SAVI, data = gb_data, importance = TRUE, ntree = ntree)
#     print(forest_1)
# }
#Determined that 1000 trees provided most stable error rate

## mtry

#code to find mtry which minimizes OOB error
data = gb_data
variables <- data.frame(data$sr_band1, data$sr_band2, data$sr_band3, data$sr_band4, data$sr_band5, data$sr_band7, data$NDVI, data$EVI, data$SAVI, data$elevation, data$slope, data$folded_aspect, data$TPI, data$TRI, data$roughness, data$greenness, data$brightness, data$wetness, data$cluster)
colnames(variables) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "NDVI", "EVI", "SAVI", "elevation", "slope", "aspect", "TPI", "TRI", "roughness", "greenness", "brightness", "wetness", "cluster")

mtry <- tuneRF(variables, variables$cluster, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)
### the optimal mtry value is 9 
variables$cluster <- as.factor(variables$cluster)
#### create test and training set for comparison to other models####
variablesplit <- variables
variablesplit$split <- sample.split(variablesplit$cluster, SplitRatio = .7)
rf_train <- subset(variablesplit, variablesplit$split == TRUE)
rf_train <- rf_train[, -20]
rf_test <- subset(variablesplit, variablesplit$split == FALSE)
rf_test <- rf_test[, -20]

#### Step 6: Run Random Forest ####


set.seed(11)

forest_1 <- randomForest(cluster ~ . , 
                         data = rf_train, importance = TRUE, ntree = 1000, mtry = 9)

forest_1


#### Step 7: Calculate Variable Importance ####

#Evaluate variable importance
varImpPlot(forest_1)

#### Step 8: Test model's performance ####
pred1 <- predict(forest_1, type = "response")

hist(pred1[, 2], breaks = 50)
plot(pred1[,1] ~ pred1[,2])
perf = prediction(pred1[,2], gb_data$cluster)

#performance in terms of true and false positive rates
pred_rf <- predict


# Area under ROC curve
auc = performance(perf, "auc")

#True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
plot(pred)
# Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

test =
rpart.plot(test, type=1)
#### ROC curve and AUC ####

pred1 <- predict(forest_1, newdata = rf_test, type = "response")

prediction1 <- prediction(pred1, rf_test$cluster)

performance1 <- performance(prediction1, "tpr", "fpr")

pred1 <- as.numeric(pred1)

roc_rf <- roc(rf_test$cluster, pred1)

auc(roc_rf)
