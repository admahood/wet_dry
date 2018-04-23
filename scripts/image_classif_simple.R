####load packages and create raster stack from tifs in local directory####
library(raster)
library(caTools)
library(randomForest)
library(ROCR)
library(party)
library(picante)
library(rpart)
library(rpart.plot)
library(tidyverse)

tif_path <- "/Volumes/seagate_external/internship_project/scene_for_classification_test/2001/2001_tifs/"

rlist=list.files(tif_path, pattern="tif$", full.names=TRUE)


for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

brick_2011 <- stack(rlist)

####display landsat image in true/false color####

landsat_rgb <- plotRGB(brick_2011, r = 3, g = 2, b = 1, axes = TRUE, stretch = 'lin', main = 'Landsat 2001 true color')
landsat_fcc <- plotRGB(brick_2011, r = 4, g = 2, b = 1, axes = TRUE, stretch = 'lin', main = 'Landsat 2001 false color')

names(brick_2011) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")

####Load and Prep plot Data with extracted pixel values####
gb_data <- read.csv("/Users/TheEagle/fire_proj/wet_dry/data/plots_with_landsat.csv")



####create cluster variable for random forest ####
#create object for cluster creation --> gb_data_clst
gb_data <- gb_data[rowSums(dplyr::select(gb_data, NonInvShru, 
                                         InvAnnGras,
                                         InvAnnFo_1,
                                         NonInvPere,
                                         SagebrushC,
                                         BareSoilCo
)) > 0,]
gb_data <- na.omit(gb_data)

gb_data_clst <- dplyr::select(gb_data, NonInvShru, 
                              InvAnnGras
                              #InvAnnFo_1
                              #NonInvPere,
                              # BareSoilCo
)
#### remove missing values for invanngrass and sagebrushcover -- necessary for making clusters
gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)
gb_data <- gb_data[rowSums(gb_data[,c(33, 40)]) > 0,]
gb_data_clst <- gb_data[,c(33, 40)]

#making clusters
comm <- decostand(gb_data_clst, method = "total")
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)

# add cluster variable to gb_data
gb_data$cluster <- as.factor(cutree(comm.bc.clust, 2))

####creating a random forest model####
#creating an object with desired variables for random forest
data = gb_data
variablesnoterrain <- data.frame(data$sr_band1, data$sr_band2, data$sr_band3, data$sr_band4, data$sr_band5, data$sr_band7, data$cluster)
colnames(variablesnoterrain) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "cluster")
variablesnoterrain$cluster <- as.factor(variablesnoterrain$cluster)

#creating test/training sets
variablesplit <- variablesnoterrain
variablesplit$split <- sample.split(variablesplit$cluster, SplitRatio = .7)
rf_train <- subset(variablesplit, variablesplit$split == TRUE)
rf_train <- rf_train[, -8]
rf_test <- subset(variablesplit, variablesplit$split == FALSE)
rf_test <- rf_test[, -8]

#train random forest model
set.seed(11)

forest_simple <- randomForest(cluster ~ . , 
                              data = rf_train, importance = TRUE, ntree = 1000, mtry = 6)

forest_simple
####plotting spectral profiles of classes ####
df <- variablesnoterrain
as.numeric(variablesnoterrain$cluster)
ms <- aggregate(df, list(df$cluster), mean)
rownames(ms) <- ms[,1]
ms <- ms[,-1]
ms <- ms[, -c(7,8,9)]
ms
#plot the mean spectra
#color vector
mycolor <- c('cyan', 'darkgreen', 'yellow', 'burlywood',
             'darkred', 'darkgray')
#change ms from data frame to matrix 
ms<- as.matrix(ms)
#create empty plot
plot(0, ylim=c(300, 4000), xlim = c(1,10), type='n', xlab="Bands", ylab = "Reflectance")
#add different classes
for(i in 1:nrow(ms)){
  lines(ms[i,], type = "l", lwd = 3, lty = 1, col = mycolor[i])
}
#title and legend
title(main = "spectral signatures sagebrush vs. cheatgrass by band")
legend("topleft", rownames(ms), cex = .8, col = mycolor, lty = 1, lwd = 3, bty = 'n')


####image classification####
#create subset of original landsat image to test classification with 
extent(brick_2011)
e <- extent(437000, 487000, 4620000, 4670000)

brick_ss <- crop(brick_2011, e)
landsat_rgb_ss <- plotRGB(brick_ss, r = 3, g = 2, b = 1, axes = TRUE, stretch = 'lin', main = 'Landsat 2001 true color subset')
writeRaster(brick_ss, )
pr <- predict(brick_ss, forest_simple, type = 'class', progress = 'text')