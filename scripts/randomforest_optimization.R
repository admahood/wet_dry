# setup ------------------------------------------------------------------------
libs <- c("caTools", "randomForest", "ROCR", "party", "caret", 
          "tidyverse", "rgdal","sf", "Metrics", "gbm", "foreach", "doParallel")

#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

date <- paste(strsplit(date()," ")[[1]][c(2,3,5)],collapse="_")
set.seed(11)

corz <- detectCores()-1

# Load and manipulate Data -----------------------------------------------------

system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 cp s3://earthlab-amahood/data/vegbank_plots_with_landsat.gpkg data/plot_data/vegbank_plots_with_landsat.gpkg")

# blm-aim data 

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(sr_band3, sr_band5),
         folded_aspect_ns = get_folded_aspect_ns(aspect)) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                ndvi=NDVI, evi=EVI, savi=SAVI,sr=SR, ndsvi,
                greenness, brightness, wetness,
                total_shrubs,
                elevation,
                folded_aspect_ns,
                slope, folded_aspect, tpi=TPI, tri=TRI, roughness, flowdir) %>%
  mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7)) %>%
  st_set_geometry(NULL)%>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7)) 
  

gtrain <- filter(gbd,split ==TRUE) %>% dplyr::select(-split)
write.csv(gtrain, paste0("gtrain_",date,".csv"))
system(paste0("aws s3 cp gtrain_",
              date,
              ".csv s3://earthlab-amahood/data/data_splits/gtrain_",
              date,".csv"))


gdevtest <- filter(gbd,split ==FALSE) %>%
  mutate(split = sample.split(split, SplitRatio=0.5))

gdev <- filter(gdevtest, split==TRUE) %>% dplyr::select(-split)
write.csv(gdev, paste0("gdev_",date,".csv"))
system(paste0("aws s3 cp gdev_",date,
              ".csv s3://earthlab-amahood/data/data_splits/gdev_",date,".csv"))

gtest <- filter(gdevtest, split==FALSE) %>% dplyr::select(-split)
write.csv(gtest, paste0("gtest_",date,".csv"))
system(paste0("aws s3 cp gtest_",date,
              ".csv s3://earthlab-amahood/data/data_splits/gtest_",date,".csv"))

#vegbank data

vbd <- st_read("data/plot_data/vegbank_plots_with_landsat.gpkg", quiet=T) %>%
  mutate(ndsvi = get_ndsvi(sr_band3, sr_band5),
         folded_aspect_ns = get_folded_aspect_ns(aspect)) %>%
  rename(total_shrubs = shrubcover, esp_mask = binary) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                ndvi, evi, savi,sr, ndsvi,
                greenness, brightness, wetness,
                total_shrubs,
                elevation,
                folded_aspect_ns,
                slope, folded_aspect, tpi, tri, roughness, flowdir)  %>%
  mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7)) %>%
  st_set_geometry(NULL) %>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7))

vtrain <- filter(vbd,split ==TRUE) %>% dplyr::select(-split)
write.csv(vtrain, paste0("vtrain_",date,".csv"))
system(paste0("aws s3 cp vtrain_",
              date,
              ".csv s3://earthlab-amahood/data/data_splits/vtrain_",
              date,".csv"))

vdevtest <- filter(vbd,split ==FALSE) %>%
  mutate(split = sample.split(split, SplitRatio=0.5))

vdev <- filter(vdevtest, split==TRUE) %>% dplyr::select(-split)
write.csv(vdev, paste0("vdev_",date,".csv"))
system(paste0("aws s3 cp vdev_",date,
              ".csv s3://earthlab-amahood/data/data_splits/vdev_",date,".csv"))

vtest <- filter(vdevtest, split==FALSE) %>% dplyr::select(-split)
write.csv(vtest, paste0("vtest_",date,".csv"))
system(paste0("aws s3 cp vtest_",date,
              ".csv s3://earthlab-amahood/data/data_splits/vtest_",date,".csv"))

# creating the hypermatrix -----------------------------------------------------
mtry <- seq(1,15,1) # 22 = # cols in the yet to be created training set
sc <- seq(3,25,1)
nodesize <- seq(1,4,1)
elevation <- c("yes","no")
folded_aspect <- c("ns","ne_sw")
dataset <- c("g", "v")

hyper_grid <- expand.grid(mtry = mtry, 
                          nodesize = nodesize, 
                          sc=sc,
                          elevation = elevation,
                          folded_aspect = folded_aspect,
                          dataset = dataset) ; nrow(hyper_grid)

# running the hypermatrix in parallel ------------------------------------------

registerDoParallel(corz)

hr <- foreach (i = 1:nrow(hyper_grid), .combine = rbind) %dopar% {
  
  if(hyper_grid$dataset[i] == "g"){
    train <- mutate(gtrain,
                  binary = as.factor(
                    ifelse(
                      total_shrubs < hyper_grid$sc[i], "Grass", "Shrub"))) %>%
      dplyr::select(-total_shrubs)}else{
        
        train <- mutate(vtrain,
                        binary = as.factor(
                          ifelse(
                            total_shrubs < hyper_grid$sc[i], "Grass", "Shrub"))) %>%
          dplyr::select(-total_shrubs)
      }
  
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
    
  if(hyper_grid$elevation[i] == "no"){dplyr::select(train,-elevation)}
  if(hyper_grid$folded_aspect[i] == "ns"){dplyr::select(train, -folded_aspect)
  }else{
      dplyr::select(train, -folded_aspect_ns)
    }
  # Train a Random Forest model
  m <- randomForest(formula = binary ~ ., 
                              data = train,
                              nodesize = hyper_grid$nodesize[i],
                              mtry = hyper_grid$mtry[i],
                              ntree = 3000)
  #validate 
  if(hyper_grid$dataset[i] == "g"){
    dev1  <- mutate(gdev,
                  binary = as.factor(
                    ifelse(
                      total_shrubs < hyper_grid$sc[i], "Grass", "Shrub")))%>%
      dplyr::select(-total_shrubs)}else{
        dev1  <- mutate(vdev,
                        binary = as.factor(
                          ifelse(
                            total_shrubs < hyper_grid$sc[i], "Grass", "Shrub")))%>%
          dplyr::select(-total_shrubs)
      }
  
  if(nrow(dev1[dev1$binary == "Grass",])<nrow(dev1[dev1$binary == "Shrub",])){
    gps <- dev1[dev1$binary == "Grass",]
    sps <- dev1[dev1$binary == "Shrub",]
    nsps <- sample_n(sps, nrow(gps))
    dev1 <- rbind(gps,nsps)
  }else{
    gps <- dev1[dev1$binary == "Grass",]
    sps <- dev1[dev1$binary == "Shrub",]
    ngps <- sample_n(gps, nrow(sps))
    dev1 <- rbind(sps,ngps)
  }
  
  if(hyper_grid$elevation[i] == "no"){dplyr::select(dev1,-elevation)}
  
  
  class_prediction <- predict(object = m,   # model object
                              newdata = dev1,  # test dataset
                              type = "class")
  
  cm <- confusionMatrix(data = class_prediction,       # predicted classes
                        reference = dev1$binary)  # actual classes
    # Store OOB error for the model                      
  w<- data.frame(
    mtry = hyper_grid$mtry[i], 
    nodesize = hyper_grid$nodesize[i], 
    sc=hyper_grid$sc[i],
    elevation = hyper_grid$elevation[i],
    oob = m$err.rate[nrow(m$err.rate), "OOB"],
    oob_grass = m$err.rate[nrow(m$err.rate), "Grass"],
    oob_shrub = m$err.rate[nrow(m$err.rate), "Shrub"],
    oob_balanced = m$err.rate[nrow(m$err.rate), "Grass"] +  m$err.rate[nrow(m$err.rate), "Shrub"],
    accuracy = as.numeric(cm$overall[1]),
    kappa = as.numeric(cm$overall[2]),
    ac_lower = as.numeric(cm$overall[3]),
    ac_upper = as.numeric(cm$overall[4]),
    ac_null = as.numeric(cm$overall[5]),
    accuracy_p = as.numeric(cm$overall[6]),
    mcnemar_p = as.numeric(cm$overall[7]),
    sensitivity = as.numeric(cm$byClass[1]),
    specificity = as.numeric(cm$byClass[2]),
    pos_pred_value = as.numeric(cm$byClass[3]),
    neg_pred_value = as.numeric(cm$byClass[4]),
    precision = as.numeric(cm$byClass[5]),
    recall = as.numeric(cm$byClass[6]),
    F1 = as.numeric(cm$byClass[7]),
    prevalence = as.numeric(cm$byClass[8]),
    detection_rate = as.numeric(cm$byClass[9]),
    detection_prevalence = as.numeric(cm$byClass[10]),
    balanced_accuracy = as.numeric(cm$byClass[11]),
    folded_aspect_type = hyper_grid$folded_aspect[i],
    dataset = hyper_grid$dataset[i]
  )
  gc()
  system(paste("echo", 
               hyper_grid$folded_aspect[i],
               round(as.numeric(cm$byClass[11]),2), 
               paste(round(i/nrow(hyper_grid)*100,2),"%")))
  return(w)
}

write.csv(hr, paste0("data/hg",date,".csv"))
system(paste0("aws s3 cp data/hg",date,".csv s3://earthlab-amahood/data/hypergrids_vb/hg", date,".csv"))

# below here is messing around, above is things that are used ------------------

# fitting the optimum model ----------------------------------------------------
read.csv("data/hg_w_elev_rf.csv")%>%
  arrange(oob) %>%
  as_tibble()

gbd <- mutate(gbd,
              binary = as.factor(
                ifelse(
                  total_shrubs < 12, "Grass", "Shrub")))

variables <- dplyr::select(gbd,
                           sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                           ndvi = NDVI, evi = EVI, savi = SAVI,sr = SR, ndsvi, #data$SATVI,
                           greenness, brightness, wetness,
                           elevation,
                           slope, folded_aspect, tpi = TPI, tri = TRI, roughness, flowdir,
                           binary)

variables$split <- sample.split(variables$binary, SplitRatio = .8) #create new variable for splitting plot data into training and test datasets (70% training data)

train <- filter(variables, split == TRUE) %>%    #create training dataset
  dplyr::select(-split)                            #remove split variable from training data
test <- filter(variables, split == FALSE) %>%
  dplyr::select(-split)

frst <- randomForest(formula = binary ~ .,
                     data = train,
                     mtry = 14,
                     nodesize = 2,
                     sampsize = 742,
                     importance=TRUE,
                     ntree = 2000)


oob_err <- frst$err.rate[nrow(frst$err.rate),]

class_prediction <- predict(object = frst,   # model object
                            newdata = test,  # test dataset
                            type = "class")
cm <- confusionMatrix(data = class_prediction,       # predicted classes
                      reference = test$binary)  # actual classes
print(cm)
paste0("Test Accuracy: ", cm$overall[1])
paste("oob accuracy")
print(1 - oob_err)

rf_auc = auc(actual = test$binary,
             predicted = class_prediction)
class_prediction <- predict(object = frst,   # model object
                            newdata = test,  # test dataset
                            type = "prob")

pred = prediction(as.numeric(class_prediction[,"Shrub"]), as.numeric(test$binary))

rocs <- performance(pred, "tpr", "fpr")
plot(rocs)
varImpPlot(frst)


# gbm: first crack--------------------------------------------------------------

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(gbd$sr_band3, gbd$sr_band5))%>%
  st_set_geometry(NULL)

gbd <- mutate(gbd, shrub = ifelse(total_shrubs > 13, 1, 0))

variables <- dplyr::select(gbd,
                           sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                           ndvi = NDVI, evi = EVI, savi = SAVI,ndsvi, #data$SATVI,
                           greenness, brightness, wetness,
                           elevation,
                           slope, folded_aspect, tpi = TPI, tri = TRI, roughness, flowdir, #data$cluster,
                           #total_shrubs)
                           shrub)

variables$split <- sample.split(variables$shrub, SplitRatio = .8) #create new variable for splitting plot data into training and test datasets (70% training data)

train <- filter(variables, split == TRUE) %>%    #create training dataset
  dplyr::select(-split)                            #remove split variable from training data
test <- filter(variables, split == FALSE) %>%
  dplyr::select(-split)

set.seed(1)
gbm_mod <- gbm(formula = shrub ~ ., 
               distribution = "bernoulli", 
               data = train,
               n.trees = 10000)

preds1 <- predict(object = gbm_mod, 
                  newdata = test,
                  n.trees = 10000)

auc(actual = test$shrub, predicted = preds1)

ntree_opt_oob <- gbm.perf(object = gbm_mod, 
                          method = "OOB", 
                          oobag.curve = TRUE)

gbm_cv <- gbm(formula = shrub ~ ., 
                       distribution = "bernoulli", 
                       data = train,
                       n.trees = 10000,
                       cv.folds = 3)

ntree_opt_cv <- gbm.perf(object = gbm_cv, 
                         method = "cv",
                         oobag.curve = T)

gbm.perf(object = gbm_mod, 
         method = "test")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_oob number of trees
preds1 <- predict(object = gbm_mod, 
                  newdata = test,
                  n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2 <- predict(object = gbm_cv, 
                  newdata = test,
                  n.trees = ntree_opt_cv)   

# Generate the test set AUCs using the two sets of preditions & compare
auc1 <- auc(actual = test$shrub, predicted = preds1)  #OOB
auc2 <- auc(actual = test$shrub, predicted = preds2)  #CV 

# hypermatrix for gbm ----------------------------------------------------------




# comparing different types of models ------------------------------------------
# Compare AUC 
print(paste0("Test set AUC (OOB): ", auc1))                         
print(paste0("Test set AUC (CV): ", auc2))

# comparing different model types

actual <- test$shrub
dt_auc <- auc(actual = actual, predicted = preds2)
bag_auc <- auc(actual = actual, predicted = preds1)
rf_auc <- auc(actual = actual, predicted = rf_preds)
gbm_auc <- auc(actual = actual, predicted = gbm_preds)

# Print results
sprintf("Decision Tree Test AUC: %.3f", dt_auc)
sprintf("Bagged Trees Test AUC: %.3f", bag_auc)
sprintf("Random Forest Test AUC: %.3f", rf_auc)
sprintf("GBM Test AUC: %.3f", gbm_auc)


# List of predictions
preds_list <- list(dt_preds, bag_preds, rf_preds, gbm_preds)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(credit_test$default), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)

# adam does not understand anything below here

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

#Determined that 1000 trees provided most stable error rate
