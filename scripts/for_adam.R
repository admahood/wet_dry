## Random Forests using Party
library(party)
library(caret)

data = read.csv(file.choose())

controls = cforest_unbiased(ntree = 5000)

## Creating decision tree and forest models, replace "Class" with your dependent variable. You get a classification
  ## tree if this variable is categorical or a regression tree if it is continuous. Both are fine, but classification
  ## trees are prerhaps a little easier to interpret
tree<-ctree(Class ~ ., data=data, controls = controls) ## Fitting the decsion tree model
plot(tree) ## Plotting this model
fit <- cforest(Class ~ ., data=data, controls = controls) ## Fitting the RF model

TreeStats<-caret:::cforestStats(tree) ## Extracting accuracy statistics for decision tree
fitStats<-caret:::cforestStats(fit) ## Extracting accuracy statistics for forest model

imp<-varimp(fit, conditional=T) ## Determining variable importance, as measured by mean decrease in accuracy of model with elimination of variable