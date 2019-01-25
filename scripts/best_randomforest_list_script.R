# Step 1: Load packages ---------------------------
libs <- c("randomForest", "dplyr","sf", "caTools")
# lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

set.seed(11)


# Step 2: Load Data --------------------------------
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/hypergrids_vb data/hypergrids")
system("aws s3 sync s3://earthlab-amahood/data/data_splits data/data_splits")

#### PAST HYPERGRID RUNS AND MODEL SELECTION DETAILS COMMENTED OUT IN THIS SECTION - DONT DELETE - NEED FOR METHODS ####
#hypergrid_original <- read.csv("data/hypergrids/hgOct_10_2018.csv") %>% arrange(desc(balanced_accuracy))


#hypergrid_combine <- read.csv("data/hypergrids/hg_a_Oct_9_2018.csv") %>% arrange(desc(balanced_accuracy))

#hypergrid_balanced <- read.csv("data/hypergrids/hgOct_12_2018.csv") %>% arrange(desc(balanced_accuracy))

#hypergrid_satvi <- read.csv("data/hypergrids/hgOct_16_2018.csv") %>% arrange(desc(balanced_accuracy))

#hypergrid_kfold  <- read.csv("data/hypergrids/hg_kfold_Nov_26_2018.csv") %>% arrange(desc(accuracy))

#hypergrid_blmvsvb <- read.csv("data/hypergrids/hgNov_23_2018.csv") %>% arrange(desc(accuracy))

#filtered_hg <- filter(hypergrid_blmvsvb, hypergrid_blmvsvb$mtry < 10) %>% 
#filter(dataset == "g") %>%
#arrange(desc(accuracy)) #%>% filter(sc == 14)
#w/elev and w/o elev both stored in same hypergrid now
#hypergrid_2 <- read.csv("data/hypergrids/hg_rf_noelev.csv") %>% arrange(oob)


#best model parameters chosen according to the following: 

# best_hyper_orig <- hypergrid_original[c(1, 4:7, 19, 25, 89, 249, 537, 621, 1194, 1537), ] %>% arrange(desc(balanced_accuracy))
# # (models with high balanced accuracy selected for middle ground and higher/lower sc value (overpredicting) models, also selected models with most extreme sc values but lower balanced accuracy (3 & 25) for visual comparison)
# best_hyper_comb <- hypergrid_combine[c(1, 7, 8, 33, 68, 387, 575, 630, 1178, 1295, 1557),] %>% arrange(desc(balanced_accuracy))
# 
# best_hyper_balanced <- hypergrid_balanced[c(1:5, 7, 9, 11, 15, 17, 22, 25, 36:40),] %>% arrange(desc(balanced_accuracy))

#best_hyper_satvi <- hypergrid_satvi[c(1:5, 7, 10, 13, 16, 17, 19, 20, 41, 49, 51),] %>% arrange(desc(balanced_accuracy))

#^^ vb_validated models to be used in all years ensemble
#are also derived from this hypergrid. as of 1/17, the models are: best_hyper_satvi[8, 11, 12]


# best_hyper_kfold <- hypergrid_kfold[c(1, 2, 10, 17, 11, 21, 30, 49),] %>% arrange(desc(accuracy))
# 
# best_hyper_blmvsvb <- hypergrid_blmvsvb[c(1, 2, 3, 5, 6, 9), ] %>% arrange(desc(accuracy))
# 
# best_hyper_aspecttype <- hypergrid_blmvsvb[c(24, 25),] %>% arrange(desc(accuracy))
# best_hyper_filtered <- filtered_hg[c(4, 7, 12, 28, 34, 43, 44, 51, 112, 175, 180),] %>% arrange(desc(accuracy)) 

    #more model selection details here 
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

hypergrid_satvi <- read.csv("data/hypergrids/hgOct_16_2018.csv") %>% arrange(desc(balanced_accuracy))
# ^^ this is the hypergrid from the most recent vb validation after splitting training data 11/26
best_hyper_satvi <- hypergrid_satvi[c(1:5, 7, 10, 13, 16, 17, 19, 20, 41, 49, 51),] %>% arrange(desc(balanced_accuracy))
# ^^ selected a variety of models to test (this is when we came up with the ensemble idea)
best10 <- best_hyper_satvi

ensemble_models <- best10[ c(12, 8, 11),]
# ^^ these are the final models selected for ensembling - one balanced (sc14), one grass biased (sc = 21), one shrub biased (sc = 9)

model_list <- list()

#model training loop using training data and splitting into balanced classes 
for(i in 1:nrow(ensemble_models)) {
  gtrain <- read.csv("data/data_splits/gtrain_Nov_26_2018.csv") %>%
    #filter(esp_mask == 1) %>%
    #mutate(total_shrubs = NonInvShru + SagebrushC) %>%
    #st_set_geometry(NULL) %>%
    mutate(binary = as.factor(ifelse(total_shrubs < ensemble_models$sc[i], "Grass", "Shrub"))
           #, ndsvi=get_ndsvi(sr_band3, sr_band5)
           ) %>%
    #dplyr::select(-X1, -total_shrubs, -ds) 
    dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                  ndvi, evi, savi, sr, ndsvi, #data$SATVI,
                  greenness, brightness, wetness,
                  elevation,
                  slope, folded_aspect, tpi, tri, roughness, flowdir, #data$cluster,
                  #total_shrubs)
                  binary) 
    #mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7))
  
  if(ensemble_models$elevation[i] == "no") {dplyr::select(gtrain,-elevation)}
  
  if(nrow(gtrain[gtrain$binary == "Grass",])<nrow(gtrain[gtrain$binary == "Shrub",])){
    gps <- gtrain[gtrain$binary == "Grass",]
    sps <- gtrain[gtrain$binary == "Shrub",]
    nsps <- sample_n(sps, nrow(gps))
    gtrain <- rbind(gps,nsps)
  }else{
    gps <- gtrain[gtrain$binary == "Grass",]
    sps <- gtrain[gtrain$binary == "Shrub",]
    ngps <- sample_n(gps, nrow(sps))
    gtrain <- rbind(sps,ngps)
  }
  
  model_list[[i]] <- randomForest(binary ~ . ,
                                  data = gtrain, 
                                  ntree = 2000, 
                                  mtry = ensemble_models$mtry[[i]], 
                                  nodesize = ensemble_models$nodesize[[i]] 
  )
  print("models created")
  
}

####create names for models based on presence of elevation variable and sc/mtry values ####
model_names <- paste("Jan18_vb_val", ifelse(ensemble_models[1:nrow(ensemble_models),]$elevation == "yes", "elev", "noelev"), "sc", ensemble_models[1:nrow(ensemble_models),]$sc,"mtry", ensemble_models[1:nrow(ensemble_models),]$mtry, "nodes", ensemble_models[1:nrow(ensemble_models),]$nodesize, sep="_")

names(model_list) <- model_names

####Model Creation Script STOPS HERE (1/25/19)####

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

