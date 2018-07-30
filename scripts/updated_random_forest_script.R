
# Step 1: Load packages ---------------------------
libs <- c("caTools", "randomForest", "ROCR", "party", "picante", "rpart", "rpart.plot", "tidyverse", "rgdal","sf")
lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

# Step 2: Load Data --------------------------------
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC) %>%
  mutate(binary = as.factor(ifelse(total_shrubs < 4, "Grass", "Shrub"))) %>%
  st_set_geometry(NULL)

# Step 6: create object with desired variables and datasets to train/test randomForest model -------------

gbd$NDSVI <- get_ndsvi(gbd$sr_band3, gbd$sr_band5)

variables <- dplyr::select(gbd,
                           sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, 
                           NDVI, EVI, SAVI, SR, NDSVI, #data$SATVI, 
                           greenness, brightness, wetness, 
                           #elevation, 
                           slope, folded_aspect, TPI, TRI, roughness, flowdir, #data$cluster, 
                           #total_shrubs)
                           binary)

variables$split <- sample.split(variables$binary, SplitRatio = .7) #create new variable for splitting plot data into training and test datasets (70% training data)

rf_train <- filter(variables, split == TRUE) %>%    #create training dataset
  dplyr::select(-split)                            #remove split variable from training data
rf_test <- filter(variables, split == FALSE) %>% 
  dplyr::select(-split)          

# Step 7: Run Random Forest ----------------------------------------------

set.seed(11)

#changed prediction label from cluster variable to binary variable
forest_1 <- randomForest(binary ~ . , 
                         data = rf_train, importance = TRUE, ntree = 5000, mtry = 9)

print(forest_1) 


#### Step 8: Calculate Variable Importance -----------------
varImpPlot(forest_1)
#### Step 9: using test data to check model performance -----------------

pred1 <- stats::predict(forest_1, newdata = rf_test, type = "response") %>%
  as.numeric()

pred <- prediction(pred1, rf_test$binary)

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

##change second argument to get different performance metrics ie false positive rate accuracy etc.##
# performance1 <- performance(prediction1, "err")


# SCRAP -----------------------------------------------------------------


#### Step 3: Set clustering variables (This part needs some work as of 6/29) -----


# gb_data <- gb_data[rowSums(dplyr::select(gb_data, NonInvShru, 
#                                          InvAnnGras,
#                                          InvAnnFo_1,
#                                          NonInvPere,
#                                          SagebrushC,
#                                          BareSoilCo
# )) > 0,]
# gb_data <- na.omit(gb_data)


# WORK ON THIS LATER FOR NARROWING DOWN BEST CLUSTERING VARIABLES (6/29)
# gb_data_clst <- dplyr::select(gb_data@data, NonInvShru, 
#InvAnnGras
#InvAnnFo_1
#NonInvPere,
# BareSoilCo)


#### remove missing values for invanngrass and sagebrushcover -- necessary for making clusters
#gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)
#gb_data <- gb_data[rowSums(gb_data@data[,c(31, 38)]) > 0,]
#gb_data_clst <- gb_data[,c(31, 38)]


# #### step 4 Finding Clusters for Veg. cover ####
# gbd_clst <- dplyr::select(gbd,
#                           BareSoilCo,
#                           InvAnnGras,
#                           InvAnnFo_1,
#                           SagebrushC,
#                           SagebrushH) %>%
#   st_set_geometry(NULL)
# 
# 
# #gbd_clst <- wisconsin(gbd_clst)
# 
# comm.bc.dist <- vegdist(gbd_clst, method = "bray")
# 
# comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# 
# plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)
# 
# # add cluster variable to gb_data
# gbd$cluster <- as.factor(cutree(comm.bc.clust, 3))




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


