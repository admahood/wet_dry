
# Step 1: Load packages ---------------------------
libs <- c("randomForest", "dplyr","sf")
# lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

set.seed(11)


# Step 2: Load Data --------------------------------
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_buffed.gpkg data/plot_data/plots_with_landsat_buffed.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/hypergrids data/hypergrids")
# which_binary <- data.frame(
#   binary_value = NA,
#   variable = NA,
#   mean_error = NA,
#   sd_error = NA
# )



hypergrid <- read.csv("data/hypergrids/hg_w_elev_rf_aug13_ntree2000.csv") %>% arrange(oob)

hypergrid_2 <- read.csv("data/hypergrids/hg_no_elev_rf_aug13_ntree2000.csv") %>% arrange(oob)


#best model parameters chosen according to the following: 

#(hyper_w_elev) -- two lowest oob errors(1,2); two lowest oob errors w/ higher shrub cover split (3, 6) -- this results in a more even dist. of error between classes
#highest shrub cover split in 25 best oob error parameter sets (25)
best_hyper <- hypergrid[c(1, 2, 3, 6, 25),] %>% arrange(oob) %>% mutate(elevation = "yes")

#(hyper_no_elev) -- two lowest oob errors(1,2); two lowest oob errors w/ higher shrub cover split (3,4) -- this results in a more even dist. of error between classes
# highest shrub cover split in 25 lowest oob error parameter sets (7)
best_hyper2 <- hypergrid_2[c(1, 2, 3, 4, 7),] %>% arrange(oob) %>% mutate(elevation = "no")

best10 <- rbind(best_hyper, best_hyper2)

#create names for models based on presence of elevation variable and sc/mtry values
model_names <- paste(ifelse(best10[1:10,]$elevation == "yes", "elev", "noelev"), "sc",best10[1:10,]$sc,"mtry", best10[1:10,]$mtry, sep="_")

model_list <- list()

for(i in 1:nrow(best10)) {

gbd <- st_read("data/plot_data/plots_with_landsat_buffed.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC) %>%
  st_set_geometry(NULL) %>%
  mutate(binary = as.factor(ifelse(total_shrubs < best10$sc[i], "Grass", "Shrub")),
                            ndsvi=get_ndsvi(sr_band3, sr_band5)) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                           ndvi, evi, savi, sr, ndsvi, #data$SATVI,
                           greenness, brightness, wetness,
                           elevation,
                           slope, folded_aspect, tpi, tri, roughness, flowdir, #data$cluster,
                           #total_shrubs)
                           binary)

if(best10$elevation[i] == "no") {dplyr::select(gbd,-elevation)}

model_list[[i]] <- randomForest(binary ~ . ,
                  data = gbd, 
                  ntree = 2000, 
                  mtry = best10$mtry[[i]], 
                  nodesize = best10$nodesize[[i]], 
                  sampsize = round(best10$sampsize[[i]]))
}

names(model_list) <- model_names

# # optimizing model- strait from data camp----------------------------------------------
# # frst
# # plot(frst) # 600 seems pretty safe
# # 
# # oob_err <- frst$err.rate[nrow(frst$err.rate),]
# # 
# # class_prediction <- predict(object = frst,   # model object
# #                             newdata = rf_test,  # test dataset
# #                             type = "class")
# # cm <- confusionMatrix(data = class_prediction,       # predicted classes
# #                       reference = rf_test$binary)  # actual classes
# # print(cm)
# # paste0("Test Accuracy: ", cm$overall[1])
# # paste("oob accuracy")
# # print(1 - oob_err)
# # 
# # #### Step 8: Calculate Variable Importance -----------------
# # varImpPlot(frst)
# 
# # adam does not understand anything below here
# 
# #### Step 9: using test data to check model performance -----------------
# 
# pred1 <- stats::predict(forest_1, newdata = rf_test, type = "response") %>%
#   as.numeric()
# 
# pred <- prediction(pred1, rf_test$binary)
# 
# ## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# perf <- performance(pred, "tpr", "fpr")
# plot(perf)
# ## precision/recall curve (x-axis: recall, y-axis: precision)
# perf1 <- performance(pred, "prec", "rec")
# plot(perf1)
# 
# ## sensitivity/specificity curve (x-axis: specificity,
# ## y-axis: sensitivity)
# perf1 <- performance(pred, "sens", "spec")
# plot(perf1)

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


