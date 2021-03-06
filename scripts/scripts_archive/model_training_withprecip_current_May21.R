#Script Title: randomForest model training with precipitation anomaly data & two methods of class labelling
#Date Last Edited: 10/11/19
#Author(s): Dylan Murphy

#### 1: Load Packages/Source scripts/set seed

libs <- c("randomForest", "dplyr","sf", "caTools", "dplyr", "caret")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE) # - optional line to install packages
source("scripts/functions.R")
set.seed(11)

#### 2: Pull data from s3: Hypergrids (for parameter selection) & Training Data 
training_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_climate_vars"


system(paste0("aws s3 sync ", training_s3_path, " data/training_timeseries")) # Use May 21 splits (most recent)

#### 3. Hypergrid paramaterization - when we run new hypergrids the parameter selection will go here ####

#### : 4. Model Training - Just for one model right now (sc = 14), but can be changed to create model ensemble group

#### 4.1: Create Training Data 

  #TWO CLASSES:
#2010 points
gtrain1 <- st_read("data/training_timeseries/manual_points_2class_2010_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#2009 points
gtrain2 <- st_read("data/training_timeseries/manual_points_2class_2009_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#2008 points
gtrain3 <- st_read("data/training_timeseries/manual_points_2class_2008_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#2007 points
gtrain4 <- st_read("data/training_timeseries/manual_points_2class_2007_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#2006 points
gtrain5 <- st_read("data/training_timeseries/manual_points_2class_2006_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#combine 2010 and 2006 points
gtrain <- rbind(gtrain1, gtrain2, gtrain3, gtrain4, gtrain5)


  #THREE CLASSES: 
#ARD 3-CLASS (GRASS, SHRUB, MIXED) TRAINING DATA (With labels from NAIP)
gtrain <- st_read("data/training_timeseries/manual_points_3class_2010_ard_phenology_all_variables_extracted_Oct9.gpkg") %>% st_set_geometry(NULL)

#### 4.1.1: old method of training class labelling - based on shrub cover attribute ####

gtrain <- gtrain %>% 
  dplyr::mutate(total_shrubs = SagebrushC) %>% 
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, ndvi, evi, savi, sr, greenness, 
                brightness, wetness, total_shrubs, elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, precip_anomaly) %>%
  dplyr::mutate(ndsvi = get_ndsvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5),
                satvi = get_satvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5, band7 = gtrain$sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = as.factor(ifelse(total_shrubs < parameters$sc, "Grass", "Shrub"))
                ) %>%
  dplyr::select(-total_shrubs, - aspect)

#Making sure that shrub and grass classes have an even number of training values to avoid bias

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


#### 4.1.2: new method of training class labelling - manually remove mixed pixels ####

#cluster variable creation commented out because labelling happens when creating the time series now - Dylan, 6/12

shrubs <- dplyr::filter(gtrain, SagebrushC  > 15  & InvAnnGras == 0) %>%mutate(cluster = 2);dim(shrubs)

grasses <- dplyr::filter(gtrain, SagebrushC == 0 & InvAnnGras > 15) %>%mutate(cluster= 1);dim(grasses)

#mixed <- dplyr::filter(gtrain, SagebrushC > 3 & InvAnnGras > 2) %>% mutate(cluster = 3);dim(mixed)

gtrain_new <- rbind(grasses,shrubs)
gtrain <- gtrain_new 
#%>% mutate(cluster = as.factor(as.numeric(label)))

greenness_vec <- c()
brightness_vec <- c()
wetness_vec <- c()
for(i in 1:length(gtrain$plot_year)) {
  if(gtrain$plot_year[i] > 2010) {
    greenness_vec[i] <- green7(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])
    brightness_vec[i] <- bright7(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])
    wetness_vec[i] <- wet7(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])}
  else {
    greenness_vec[i] <- green5(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])
    brightness_vec[i] <- bright5(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])
    wetness_vec [i] <- wet5(band1 = gtrain$sr_band1[i], band2 = gtrain$sr_band2[i], band3 = gtrain$sr_band3[i], band4 = gtrain$sr_band4[i], band5 = gtrain$sr_band5[i], band7 = gtrain$sr_band7[i])
  }
}

gtrain <- gtrain %>% 
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, 
                #cluster, 
                precip_anomaly, plot_year,
                label) %>%
  dplyr::mutate(ndvi = get_ndvi(band3 = gtrain$sr_band3, band4 = gtrain$sr_band4),
                evi = get_evi(band1 = gtrain$sr_band1, band3 = gtrain$sr_band3, band4 = gtrain$sr_band4),
                savi = get_savi(band3 = gtrain$sr_band3, band4 = gtrain$sr_band4),
                sr = get_sr(band3 = gtrain$sr_band3, band4 = gtrain$sr_band4),
                greenness = greenness_vec,
                brightness = brightness_vec,
                wetness = wetness_vec,
                ndsvi = get_ndsvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5),
                satvi = get_satvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5, band7 = gtrain$sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = as.factor(gtrain$label)
                ) %>%
  dplyr::select(#-cluster,
                -aspect, -elevation, -plot_year)

if(nrow(gtrain[gtrain$binary == "grass",])<nrow(gtrain[gtrain$binary == "shrub",])){
  gps <- gtrain[gtrain$binary == "grass",]
  sps <- gtrain[gtrain$binary == "shrub",]
  mps <- gtrain[gtrain$binary == "mixed",]
  nsps <- sample_n(sps, nrow(gps))
  gtrain <- rbind(gps,nsps, mps)
}else{
  gps <- gtrain[gtrain$binary == "grass",]
  sps <- gtrain[gtrain$binary == "shrub",]
  mps <- gtrain[gtrain$binary == "mixed",]
  ngps <- sample_n(gps, nrow(sps))
  gtrain <- rbind(sps,ngps, mps)
}

if(nrow(gtrain[gtrain$binary == "mixed",]) > nrow(gtrain[gtrain$binary == "shrub",])) {
  sps <- gtrain[gtrain$binary == "shrub",]
  gps <- gtrain[gtrain$binary == "grass",]
  mps <- gtrain[gtrain$binary == "mixed",]
  nmps <- sample_n(mps, nrow(sps))
  gtrain <- rbind(sps, nmps, gps)
}

gtrain <- dplyr::select(gtrain, -binary)

#### 4.1.3: create training data using manually created points (already labelled) ####
gtrain <- gtrain %>% 
  # dplyr::mutate(total_shrubs = SagebrushC) %>% 
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, ndvi, evi, savi, sr, greenness, 
                brightness, wetness, 
                #total_shrubs, 
                elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, 
                precip_anomaly, Label) %>%
  dplyr::mutate(ndsvi = get_ndsvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5),
                satvi = get_satvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5, band7 = gtrain$sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = Label
                #binary = as.factor(ifelse(total_shrubs < parameters$sc, "Grass", "Shrub"))
  ) %>%
  dplyr::select(#-total_shrubs, 
    - aspect,
    -Label)
#### 4.1.4: USE THIS CODE AS OF SEP 27 landsat ARD extent training data with differenced veg indices (already labelled) ####
gtrain <- gtrain %>% 
  # dplyr::mutate(total_shrubs = SagebrushC) %>% 
  dplyr::select(spring_sr_band1, spring_sr_band2, spring_sr_band3, spring_sr_band4, spring_sr_band5, spring_sr_band7,
                summer_sr_band1, summer_sr_band2, summer_sr_band3, summer_sr_band4, summer_sr_band5, summer_sr_band7,
                spring_ndvi, spring_evi, spring_savi, spring_sr, spring_ndti, spring_sla_index, spring_ndi7, spring_green_ndvi, 
                summer_ndvi, summer_evi, summer_savi, summer_sr, summer_ndti, summer_sla_index, summer_ndi7, summer_green_ndvi,
                spring_greenness, spring_brightness, spring_wetness, summer_greenness, summer_brightness, summer_wetness,
                #total_shrubs, 
                elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, 
                precip_anomaly, 
                diff_evi, diff_ndsvi, diff_savi, diff_ndvi, diff_satvi, diff_sr, diff_ndti, diff_green_ndvi, diff_sla_index, diff_ndi7,
                jan_precip, feb_precip, mar_precip, apr_precip, may_precip,
                jun_precip, jul_precip, aug_precip, sep_precip, oct_precip, nov_precip, dec_precip,
                winter_precip, spring_precip, summer_precip, fall_precip,
                aet_z, def_z, tmn_z,
                Label) %>%
  dplyr::mutate(spring_ndsvi = get_ndsvi(band3 = gtrain$spring_sr_band3, band5 = gtrain$spring_sr_band5),
                summer_ndsvi = get_ndsvi(band3 = gtrain$summer_sr_band3, band5 = gtrain$summer_sr_band5),
                spring_satvi = get_satvi(band3 = gtrain$spring_sr_band3, band5 = gtrain$spring_sr_band5, band7 = gtrain$spring_sr_band7, L = 0.5),
                summer_satvi = get_satvi(band3 = gtrain$summer_sr_band3, band5 = gtrain$summer_sr_band5, band7 = gtrain$summer_sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = Label
                #binary = as.factor(ifelse(total_shrubs < parameters$sc, "Grass", "Shrub"))
  ) %>%
  dplyr::select(#-total_shrubs, 
    - aspect,
    -Label)

#### 4.2 (IN PROGRESS): Random Forest Model Tuning ####
ddd<- list()
mods<-list()
#ddd[[1]] <- cbind(clm,dplyr::select(resp,cluster)) 
ddd[[1]] <- gtrain
rvars <- ncol(ddd[[1]])-2

control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3,
                        search="grid")
var_results <- data.frame(accuracy=NA, nvars = NA, mean_acc=NA,sd_acc=NA, dropped = NA)
for (i in 1:rvars){ # this takes 10-30 minutes
  t0<-Sys.time()
  tgrid <- expand.grid(
    .mtry = 1:round(sqrt(ncol(ddd[[i]])-2)), #cluster and geom don't count
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  # next time this is ran, do rf[[i]] to be able to look at the models later
  mods[[i]] <- train(binary~., 
                     data=ddd[[i]],#st_set_geometry(ddd[[i]], NULL), 
                     method='ranger',
                     metric=c('Accuracy'), # or RMSE?
                     tuneGrid=tgrid, 
                     trControl=control,
                     #case.weights = www, # not sure why this doesn't work
                     importance = "permutation")
  
  var_results[i, 1] <- max(mods[[i]]$results$Accuracy)
  var_results[i, 2] <- i
  var_results[i, 3] <- mean(mods[[i]]$results$Accuracy)
  var_results[i, 4] <- sd(mods[[i]]$results$Accuracy)
  
  
  least_important <- caret::varImp(mods[[i]])$importance %>%
    rownames_to_column("var") %>%
    arrange(Overall)
  vvv <- least_important[1,1]
  ddd[[i+1]] <- dplyr::select(ddd[[i]],-vvv)
  
  var_results[i, 5] <- vvv
  print(paste("Progress:", round(i/rvars*100), "% |",
              "Accuracy:",  round(max(mods[[i]]$results$Accuracy)*100),
              "% | Dropped", vvv, Sys.time()-t0))
}
#### 4.3: Random Forest Model Training ####


model2 <- randomForest(binary ~ . ,
                      data = gtrain, 
                      ntree = 5000, 
                      mtry = 10, 
                      nodesize = 4,
                      na.action = na.exclude
)
#### END ####

