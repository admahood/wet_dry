# setup ---------------------------
libs <- c("caTools", "randomForest", "ROCR", "party", "caret", 
          "tidyverse", "rgdal","sf", "Metrics", "gbm", "foreach", "parallel")

#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

set.seed(11)

corz <- detectCores()-1
# Load and manipulate Data ---------------------------------------------
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 cp s3://earthlab-amahood/data/vegbank_plots_with_landsat.gpkg data/plot_data/vegbank_plots_with_landsat.gpkg")

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(sr_band3, sr_band5)) %>%
  st_set_geometry(NULL)

vbd <- st_read("data/plot_data/vegbank_plots_with_landsat.gpkg", quiet=T) %>%
  mutate(ndsvi = get_ndsvi(sr_band3, sr_band5)) %>%
  rename(total_shrubs = shrubcover) %>%
  st_set_geometry(NULL)

# tuning with hypermatrix-------------------------------------------------------------------
mtry <- seq(1,10,1) # 22 = # cols in the yet to be created training set
nodesize <- seq(1, 7, 1)
sampsize <- round(nrow(gbd)*0.8 * c(0.632))
sc <- seq(8,25,1)
elevation <- c("yes","no")

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, 
                          nodesize = nodesize, 
                          sampsize = sampsize,
                          sc=sc,
                          elevation = elevation,
                          oob = NA,
                          oob_grass = NA,
                          oob_shrub = NA,
                          accuracy = NA,
                          accuracy_p = NA,
                          mcnemar_p = NA,
                          balanced_accuracy = NA) ; nrow(hyper_grid)
# note to self parallelize this
# 
# with elevation ---------------------------------------------------------------

# models <- list()
registerDoParallel(2)

hr <- foreach (i = 1:4, .combine = rbind) %dopar% {
  
  train <- mutate(gbd,
                binary = as.factor(
                  ifelse(
                    total_shrubs < hyper_grid$sc[i], "Grass", "Shrub")))%>%
    dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                  ndvi=NDVI, evi=EVI, savi=SAVI,sr=SR, ndsvi,
                  greenness, brightness, wetness,
                  elevation,
                  slope, folded_aspect, tpi=TPI, tri=TRI, roughness, flowdir,
                  binary)
  if(hyper_grid$elevation[i] == "no"){dplyr::select(train,-elevation)}
  
  # Train a Random Forest model
  m <- randomForest(formula = binary ~ ., 
                              data = train,
                              mtry = hyper_grid$mtry[i],
                              nodesize = hyper_grid$nodesize[i],
                              sampsize = hyper_grid$sampsize[i],
                              ntree = 3000)
  #validate with vegbank
  test  <- mutate(vbd,
                binary = as.factor(
                  ifelse(
                    total_shrubs < hyper_grid$sc[i], "Grass", "Shrub")))%>%
    dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                  ndvi, evi, savi,sr, ndsvi,
                  greenness, brightness, wetness,
                  elevation,
                  slope, folded_aspect, tpi, tri, roughness, flowdir,
                  binary)  
  if(hyper_grid$elevation[i] == "no"){dplyr::select(test,-elevation)}
  
  
  class_prediction <- predict(object = m,   # model object
                              newdata = test,  # test dataset
                              type = "class")
  
  cm <- confusionMatrix(data = class_prediction,       # predicted classes
                        reference = test$binary)  # actual classes
    # Store OOB error for the model                      
  w<- data.frame(
    mtry = hyper_grid$mtry[i], 
    nodesize = hyper_grid$nodesize[i], 
    sampsize = hyper_grid$sampsize[i],
    sc=hyper_grid$sc[i],
    elevation = elevation[i],
    oob = m$err.rate[nrow(m$err.rate), "OOB"],
    oob_grass = m$err.rate[nrow(m$err.rate), "Grass"],
    oob_shrub = m$err.rate[nrow(m$err.rate), "Shrub"],
    comb_err = m$err.rate[nrow(m$err.rate), "Grass"] +  m$err.rate[nrow(m$err.rate), "Shrub"],
    accuracy = as.numeric(cm$overall[1]),
    kappa = as.numeric(cm$overall[2]),
    ac_lower = as.numeric(cm$overall[3]),
    ac_upper = as.numeric(cm$overall[4]),
    ac_null = as.numeric(cm$overall[5]),
    accuracy_p = as.numeric(cm$overall[6]),
    mcnemar_p = as.numeric(cm$overall[7]),
    sensitivity = as.numeric(cm$byClass[1]),
    specificity = as.numeric(cm$byClass[2]),
    precision = as.numeric(cm$byClass[4]),
    balanced_accuracy = as.numeric(cm$byClass[11])
  )
  gc()
  system(paste("echo", as.numeric(cm$byClass[11]), paste(round(i/nrow(hyper_grid)*100,2),"%")))
  return(w)
}

write.csv(hr, "data/hg_w_elev_vb.csv")
system("aws s3 cp data/hg_w_elev_vb.csv s3://earthlab-amahood/data/hypergrids_vb/hg_w_elev_vb.csv")

# checking out which models are best -------------------------------------------
hg_w_elev <- read.csv("data/hg_w_elev_rf.csv")
arrange(hg_w_elev, oob) %>% as_tibble()


opt_i <- which.min(hyper_grid$oob)
models[[opt_i]]
hyper_grid <- arrange(hyper_grid,oob) %>% as_tibble()
hg2 <- arrange(hyper_grid, comb_err) %>%
  filter(oob_grass < 0.2 & oob_shrub < 0.2)


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
