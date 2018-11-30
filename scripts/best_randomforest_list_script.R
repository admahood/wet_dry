# Step 1: Load packages ---------------------------
libs <- c("randomForest", "dplyr","sf", "caTools")
# lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

set.seed(11)


# Step 2: Load Data --------------------------------
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/hypergrids_vb data/hypergrids")
# which_binary <- data.frame(
#   binary_value = NA,
#   variable = NA,
#   mean_error = NA,
#   sd_error = NA
# )



hypergrid_original <- read.csv("data/hypergrids/hgOct_10_2018.csv") %>% arrange(desc(balanced_accuracy))


hypergrid_combine <- read.csv("data/hypergrids/hg_a_Oct_9_2018.csv") %>% arrange(desc(balanced_accuracy))

hypergrid_balanced <- read.csv("data/hypergrids/hgOct_12_2018.csv") %>% arrange(desc(balanced_accuracy))

hypergrid_satvi <- read.csv("data/hypergrids/hgOct_16_2018.csv") %>% arrange(desc(balanced_accuracy))

hypergrid_kfold  <- read.csv("data/hypergrids/hg_kfold_Nov_26_2018.csv") %>% arrange(desc(accuracy))

hypergrid_blmvsvb <- read.csv("data/hypergrids/hgNov_23_2018.csv") %>% arrange(desc(accuracy))

filtered_hg <- filter(hypergrid_blmvsvb, hypergrid_blmvsvb$mtry < 10) %>% 
  filter(dataset == "g") %>%
  arrange(desc(accuracy)) #%>% filter(sc == 14)
#w/elev and w/o elev both stored in same hypergrid now
#hypergrid_2 <- read.csv("data/hypergrids/hg_rf_noelev.csv") %>% arrange(oob)


#best model parameters chosen according to the following: 

#Oct 9 - Testing overpredicting models & data split methods

best_hyper_orig <- hypergrid_original[c(1, 4:7, 19, 25, 89, 249, 537, 621, 1194, 1537), ] %>% arrange(desc(balanced_accuracy))
# (models with high balanced accuracy selected for middle ground and higher/lower sc value (overpredicting) models, also selected models with most extreme sc values but lower balanced accuracy (3 & 25) for visual comparison)
best_hyper_comb <- hypergrid_combine[c(1, 7, 8, 33, 68, 387, 575, 630, 1178, 1295, 1557),] %>% arrange(desc(balanced_accuracy))

best_hyper_balanced <- hypergrid_balanced[c(1:5, 7, 9, 11, 15, 17, 22, 25, 36:40),] %>% arrange(desc(balanced_accuracy))

best_hyper_satvi <- hypergrid_satvi[c(1:5, 7, 10, 13, 16, 17, 19, 20, 41, 49, 51),] %>% arrange(desc(balanced_accuracy))

best_hyper_kfold <- hypergrid_kfold[c(1, 2, 10, 17, 11, 21, 30, 49),] %>% arrange(desc(accuracy))

best_hyper_blmvsvb <- hypergrid_blmvsvb[c(1, 2, 3, 5, 6, 9), ] %>% arrange(desc(accuracy))

best_hyper_filtered <- filtered_hg[c(4, 7, 12, 28, 34, 43, 44, 51, 112, 175, 180),] %>% arrange(desc(accuracy)) 
####model testing/selection history ####
#(hyper_w_elev) -- two lowest oob errors(1,2); two lowest oob errors w/ higher shrub cover split (3, 6) -- this results in a more even dist. of error between classes
#highest shrub cover split in 25 best oob error parameter sets (25)
#best_hyper <- hypergrid[c(1, 2, 6, 10, 12),] %>% arrange(oob) %>% mutate(elevation = "yes")
#^^^disregard above, looking for models with highest balanced accuracy now


#top 5 balanced accuracies with and without elevation -- Oct 2
# best_hyper <- hypergrid[c(1:8, 10, 12),] %>% arrange(desc(balanced_accuracy))

#best hyper (buffered) - so we know which numbers we used for buffered hypergrids
#best_hyper <- hypergrid[c(1, 2, 3, 6, 25),] %>% arrange(oob) %>% mutate(elevation = "yes")

#(hyper_no_elev) -- two lowest oob errors(1,2); two lowest oob errors w/ higher shrub cover split (3,4) -- this results in a more even dist. of error between classes
# highest shrub cover split in 25 lowest oob error parameter sets (7)
#best_hyper2 <- hypergrid_2[c(1, 2, 7, 13, 14),] %>% arrange(oob) %>% mutate(elevation = "no")

#best hyper (buffered) no elevation: so we know which numbers we used for buffered hypergrids
#best_hyper2 <- hypergrid_2[c(1, 2, 3, 4, 7),] %>% arrange(oob) %>% mutate(elevation = "no")

#best10 <- rbind(best_hyper, best_hyper2)
#### model list application ####
best10 <- best_hyper_blmvsvb

ensemble_models <- best10[ c(1, 8, 11),]


model_list <- list()

for(i in 1:nrow(best10)) {
  
  gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
    filter(esp_mask == 1) %>%
    mutate(total_shrubs = NonInvShru + SagebrushC) %>%
    st_set_geometry(NULL) %>%
    mutate(binary = as.factor(ifelse(total_shrubs < best10$sc[i], "Grass", "Shrub")),
           ndsvi=get_ndsvi(sr_band3, sr_band5)) %>%
    #dplyr::select(-X1, -total_shrubs, -ds) 
    dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                  ndvi = NDVI, evi = EVI, savi = SAVI, sr = SR, ndsvi = NDSVI, #data$SATVI,
                  greenness, brightness, wetness,
                  elevation,
                  slope, folded_aspect, tpi = TPI, tri = TRI, roughness, flowdir, #data$cluster,
                  #total_shrubs)
                  binary) %>%
    mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7))
  
  if(best10$elevation[i] == "no") {dplyr::select(gbd,-elevation)}
  
  model_list[[i]] <- randomForest(binary ~ . ,
                                  data = gbd, 
                                  ntree = 2000, 
                                  mtry = best10$mtry[[i]], 
                                  nodesize = best10$nodesize[[i]], 
  )
  print("models created")
  
}

#create names for models based on presence of elevation variable and sc/mtry values
model_names <- paste("nov23_train", ifelse(best10[1:nrow(best10),]$elevation == "yes", "elev", "noelev"), "sc", best10[1:nrow(best10),]$sc,"mtry", best10[1:nrow(best10),]$mtry, "nodes", best10[1:nrow(best10),]$nodesize, sep="_")
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

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(sr_band3, sr_band5)
         ) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                ndvi=NDVI, evi=EVI, savi=SAVI,sr=SR, ndsvi,
                greenness, brightness, wetness,
                total_shrubs,
                elevation,
                slope, folded_aspect, tpi=TPI, tri=TRI, roughness, flowdir) %>%
  mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7)) %>%
  st_set_geometry(NULL)%>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7))  
  

gtrain <- filter(gbd,split ==TRUE) %>% dplyr::select(-split) 



for(i in 1:nrow(best10)) {
  
  train <- gtrain %>% 
    mutate(binary = as.factor(ifelse(total_shrubs < best10$sc[i], "Grass", "Shrub"))) %>%
    dplyr::select(-total_shrubs)
  
  if(best10$elevation[i] == "no") {dplyr::select(train,-elevation)}
  
  if(nrow(train[train$binary == "Grass",])<nrow(train[train$binary == "Shrub",])){
    gps <- train[train$binary == "Grass",]
    sps <- train[train$binary == "Shrub",]
    nsps <- sample_n(sps, nrow(gps))
    train <- rbind(gps,nsps)
  }else{
    gps <- train[train$binary == "Grass",]
    sps <- train[train$binary == "Shrub",]
    ngps <- sample_n(gps, nrow(sps))
    train <- rbind(sps,ngps)
  }
  
  model_list[[i]] <- randomForest(binary ~ . ,
                                  data = train, 
                                  ntree = 2000, 
                                  mtry = best10$mtry[[i]], 
                                  nodesize = best10$nodesize[[i]] 
  )
  print("models created")
  
}


