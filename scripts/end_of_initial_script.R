


#### train landsat pixels as shrub vs grass ####

# Dylan's progress starts here

#libraries
install.packages("randomForest"); install.packages("party"); 
library(randomForest); 
library(rpart); 
library(rattle); 
library(rpart.plot); 
library(RColorBrewer); 
library(caret); 
library(rfUtilities)
library(ROCR)


#load and prep data
gb_data <- read.csv("data/plots_with_landsat.csv")
gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)



## for splitting data into test and training sets, might not need this since random forests do the splitting and sampling within the function
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splits.gb_plots <- splitdf(gb_plots, seed = 1)

trainset.gb_plots <- splits.gb_plots$trainset; testset.gb_plots <- splits.gb_plots$testset




# end of Dylan's work

library(picante)

s = read.csv("data/plots_with_landsat.csv")
t = s[rowSums(s[,c(32,39)]) > 0,]
u = t[,c(32,39)]

comm <- decostand(u, method = "total")
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)
t$cluster <- as.factor(cutree(comm.bc.clust, 2))



library(randomForest)
## Random Forests using Party
library(party)
library(caret)

data = t

controls = cforest_unbiased(ntree = 500)

## Creating decision tree and forest models, replace "Class" with your dependent variable. You get a classification
## tree if this variable is categorical or a regression tree if it is continuous. Both are fine, but classification
## trees are prerhaps a little easier to interpret
tree<-ctree(cluster ~ sr_band1 +sr_band2 +sr_band3 +sr_band4 +sr_band5 + sr_band7, data= data, controls = controls) ## Fitting the decsion tree model
plot(tree) ## Plotting this model
fit <- cforest(cluster ~ ., data=data, controls = controls) ## Fitting the RF model

TreeStats<-caret:::cforestStats(tree) ## Extracting accuracy statistics for decision tree
fitStats<-caret:::cforestStats(fit) ## Extracting accuracy statistics for forest model

imp<-varimp(fit, conditional=T) ## Determining variable importance, as measured by mean decrease in accuracy of model with elimination of variable

