library(picante)
library(foreign)

d = read.dbf("projects/wet_dry/data/gb_plots/gb_plots.dbf")

s = d[,c(31,32,38)]
s = s[rowSums(s) > 0,]

comm <- decostand(s, method = "total")
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)
s$cluster <- as.factor(cutree(comm.bc.clust, 2))



library(randomForest)
## Random Forests using Party
library(party)
library(caret)

data = s

controls = cforest_unbiased(ntree = 5000)

## Creating decision tree and forest models, replace "Class" with your dependent variable. You get a classification
## tree if this variable is categorical or a regression tree if it is continuous. Both are fine, but classification
## trees are prerhaps a little easier to interpret
tree<-ctree(cluster ~ ., data=data, controls = controls) ## Fitting the decsion tree model
plot(tree) ## Plotting this model
fit <- cforest(cluster ~ ., data=data, controls = controls) ## Fitting the RF model

TreeStats<-caret:::cforestStats(tree) ## Extracting accuracy statistics for decision tree
fitStats<-caret:::cforestStats(fit) ## Extracting accuracy statistics for forest model

imp<-varimp(fit, conditional=T) ## Determining variable importance, as measured by mean decrease in accuracy of model with elimination of variable
