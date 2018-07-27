
#### Step 1: Load packages####
libs <- c("caTools", "randomForest", "ROCR", "party", "picante", "rpart", "rpart.plot", "tidyverse", "rgdal")
lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
#### Step 2: Load Data --------------------
system("aws s3 sync s3://earthlab-amahood/data/csv data/plot_data")

gb_data <- readOGR("data/plot_data/plots_with_landsat.gpkg")
#### Step 3: Set clustering variables (This part needs some work as of 6/29) -----


gb_data <- gb_data[rowSums(dplyr::select(gb_data@data, NonInvShru, 
                                         InvAnnGras,
                                         InvAnnFo_1,
                                         NonInvPere,
                                         SagebrushC,
                                         BareSoilCo
)) > 0,]
gb_data <- na.omit(gb_data)


# WORK ON THIS LATER FOR NARROWING DOWN BEST CLUSTERING VARIABLES (6/29)
# gb_data_clst <- dplyr::select(gb_data@data, NonInvShru, 
                              #InvAnnGras
                              #InvAnnFo_1
                              #NonInvPere,
                              # BareSoilCo)
                              
                  
#### remove missing values for invanngrass and sagebrushcover -- necessary for making clusters
gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)
gb_data <- gb_data[rowSums(gb_data@data[,c(31, 38)]) > 0,]
gb_data_clst <- gb_data[,c(31, 38)]

#### step 4 Finding Clusters for Veg. cover ####

comm <- decostand(gb_data_clst@data, method = "total")

comm.bc.dist <- vegdist(comm, method = "bray")

comm.bc.clust <- hclust(comm.bc.dist, method = "average")

plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)

# add cluster variable to gb_data
gb_data$cluster <- as.factor(cutree(comm.bc.clust, 2))


gb_data$binary <- as.factor(ifelse(gb_data$NonInvShru < 5, "Grass", "Shrub"))


gb_data<- gb_data[gb_data$esp_mask == 1, ]



#### step 5: Determine proper parameters for ntree and mtry - Another step which can be refined later  - for now everything is commented out (6/29) ####

#### for loop for testing number of trees

# ntree = 2500
#  for (i in 1:10) {
#     forest_1 <- randomForest(cluster ~ sr_band1 +sr_band2 +sr_band3 +sr_band4 +sr_band5 + sr_band7 + NDVI + EVI + SAVI, data = gb_data, importance = TRUE, ntree = ntree)
#     print(forest_1)
# }

#Determined that 1000 trees provided most stable error rate

# ####code to find mtry value which minimizes error
#
# data = gb_data@data
# variables <- data.frame(data$sr_band1, data$sr_band2, data$sr_band3, data$sr_band4, data$sr_band5, data$sr_band7, data$NDVI, data$EVI, data$SAVI, data$elevation, data$slope, data$folded_aspect, data$TPI, data$TRI, data$roughness, data$greenness, data$brightness, data$wetness, data$cluster)
# colnames(variables) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "NDVI", "EVI", "SAVI", "elevation", "slope", "aspect", "TPI", "TRI", "roughness", "greenness", "brightness", "wetness", "cluster")
# 
# mtry <- tuneRF(variables, variables$cluster, ntreeTry=1000,
#                stepFactor=1.5,improve= T, trace=TRUE, plot=TRUE)
# 
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# 
# print(mtry)
# print(best.m)

### the optimal mtry value is 9 

#### step 6: create object with desired variables and datasets to train/test randomForest model -------------
#creating an object with desired variables for random forest
data = gb_data@data
variables <- data.frame(data$sr_band1, data$sr_band2, data$sr_band3, data$sr_band4, data$sr_band5, data$sr_band7, data$NDVI, data$EVI, 
                        data$SAVI, data$SR, #data$SATVI, 
                        data$NDSVI, data$greenness, data$brightness, data$wetness, data$elevation, 
                        data$slope, 
                        data$aspect, data$TPI, data$TRI, data$roughness, data$flowdir, #data$cluster, 
                        data$binary)
colnames(variables) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "ndvi", "evi", "savi", "sr", #"satvi", 
                         "ndsvi", "greenness", "brightness", "wetness", "elevation",
                         "slope", "aspect",
                                  "tpi", "tri", "roughness", "flowdir", #"cluster",
                         "binary")
variables$cluster <- as.factor(variables$cluster)

variables$binary <- as.factor(variables$binary)

variablesplit <- variables
variablesplit$split <- sample.split(variablesplit$binary, SplitRatio = .7) #create new variable for splitting plot data into training and test datasets (70% training data)

rf_train <- subset(variablesplit, variablesplit$split == TRUE) #create training dataset
rf_train <- rf_train[, - 23] #remove split variable from training data
rf_test <- subset(variablesplit, variablesplit$split == FALSE) #create test data
rf_test <- rf_test[, -23] #remove split variable from test data

#### Step 7: Run Random Forest ####


set.seed(11)

#changed prediction label from cluster variable to binary variable
forest_1 <- randomForest(binary ~ . , 
                         data = rf_train, importance = TRUE, ntree = 1000, mtry = 9)

forest_1


#### Step 8: Calculate Variable Importance -----------------
varImpPlot(forest_1)
#### Step 9: using test data to check model performance -----------------

# pred1 <- predict(forest_1, newdata = rf_test, type = "response")
# pred1 <- as.vector(pred1)
# pred1 <- as.numeric(pred1)
# rf_test$cluster <- as.numeric(rf_test$cluster)
# test_data <- rf_test$cluster
# 
# prediction1 <- prediction(pred1, rf_test$cluster)

##change second argument to get different performance metrics ie false positive rate accuracy etc.##
# performance1 <- performance(prediction1, "err")


