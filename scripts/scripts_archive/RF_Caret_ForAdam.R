#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# Script created by Nathan Mietkiewicz
# Last edited on November 20th, 2015
#
# This script specifically employs the caret package to run classification random forests and classification trees based on 
# a 10-fold cross validation model that is repeated 10 times.  This model is evaluated using cross validation techniques  
# based on predict accuracies.
#------------------------------------------------------------------------------------------------------------------------

# Set working directory, load data, and load libraries
library(rpart); library(rattle); library(rpart.plot); library(RColorBrewer); library(caret); library(rfUtilities)

# setwd("~/Dropbox/PhD Program/ClassSemester/2015_Fall/Adv_BioStats/Aspen_Regen/Aspen_Regen")
# aspen <- read.csv("~/Dropbox/PhD Program/ClassSemester/2015_Fall/Adv_BioStats/Aspen_Regen/Aspen_Regen/aspen_hyp2.csv")
#------------------------------------------------------------------------------------------------------------------------
#Data prep
# aspen$nd <- as.factor(aspen$nd); aspen$casp <- as.numeric(aspen$casp)
# aspen$steep <- as.numeric(aspen$steep); aspen$bls <- as.numeric(aspen$bls)
# aspen$logsa <- log1p(aspen$sa)

mrf = 

#------------------------------------------------------------------------------------------------------------------------
#Subset the data into training and testing data
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
#splits.aspen <- splitdf(aspen, seed=532)



training.mrf <- splits.mrf$trainset; testing.mrf <- splits.mrf$testset

#------------------------------------------------------------------------------------------------------------------------
#Random forests that predit the number of disturbances - Classification Random Forests
#------------------------------------------------------------------------------------------------------------------------
#Prep the data and check for multi-collinearity
trainingCor <- multi.collinear(training.mrf); trainingCor

# Setting the parameters for model preprocessing and tuning from the caret package: 
fitControl <- trainControl(## 10-fold Crossvalidation
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 5,
  verboseIter=FALSE ,
  # PCA Preprocessing
  preProcOptions="pca",
  # With parallel backend
  allowParallel=TRUE)

# Model Training for CART and random forests
set.seed(981)
# CART <- train(nd ~ st + logsa + pf + bls + yo + casp + el + steep + de, #This is the classification tree
#               data=training.aspen, method="rpart", trControl= fitControl) 
# RF <- train(nd ~ st + logsa + pf + bls + yo + casp + el + steep + de,  #This is the random forest
#             data = training.aspen, method = "rf", trControl= fitControl, importance=T) 
# #Note importance in the RF model: this will produce an importance plot for each of your factors
# RF2 <- train(nd ~ st + logsa + pf + bls + yo + casp + el + steep + de,       
#              data = training.aspen, method = "rf", trControl= fitControl)

CART <- train(KC ~ ., #This is the classification tree
              data=training.mrf, method="rpart", trControl= fitControl) 
RF <- train(sum_spp ~ .,  #This is the random forest
            data = training.mrf[,-14], method = "rf", trControl= fitControl, importance=T) 
#Note importance in the RF2 model: this will produce an importance plot for the overall model

pdf(file= "CART_all.pdf", height = 8, width = 9) # This will write your classification tree results to a pdf
fancyRpartPlot(CART$finalModel, main="CART Model")
plot(CART$finalModel) #Plain CT - no text

plot(CART$finalModel) #Plain CT but text will be written to the splits/branches
text(CART$finalModel)
dev.off()

# Measuring Model Accuracy
Model <- c("CART", "Random Forest")
Accuracy <- c(round(max(CART$results$Accuracy),4)*100,round(max(RF$results$Accuracy),4)*100)
Performance <- cbind(Model,Accuracy); Performance

# Printing the Confusion Matrix
pred_CART <- predict(CART, testing.mrf); CM_CART <- table(pred_CART, testing.mrf$FF) 
pred_RF <- predict(RF, testing.mrf); CM_RF <- table(pred_RF, testing.mrf$FF)
print("CART"); CM_CART; print("Random Forest"); CM_RF

# Most significant variables in the model
VarImportance <- varImp(RF); plot(VarImportance)
VarImportance <- varImp(RF2); plot(VarImportance)

# Plot the top 15 predictors
plot(VarImportance, main = "Top 15 most influencial Predictors", top = 15)
predictions <- predict(RF, newdata=testing.mrf); predictions
