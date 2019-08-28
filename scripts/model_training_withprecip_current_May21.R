#Script Title: randomForest model training with precipitation anomaly data & two methods of class labelling
#Date Last Edited: 8/7/19
#Author(s): Dylan Murphy

#### 1: Load Packages/Source scripts/set seed

libs <- c("randomForest", "dplyr","sf", "caTools", "dplyr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE) # - optional line to install packages
source("scripts/functions.R")
set.seed(11)

#### 2: Pull data from s3: Hypergrids (for parameter selection) & Training Data 
training_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/ard_pheno_spatially_balanced_points"


system(paste0("aws s3 sync ", training_s3_path, " data/training_timeseries")) # Use May 21 splits (most recent)

#### 3. Hypergrid paramaterization - when we run new hypergrids the parameter selection will go here ####

#### : 4. Model Training - Just for one model right now (sc = 14), but can be changed to create model ensemble group

#### 4.1: Create Training Data 

#2010 points
gtrain1 <- st_read("data/training_timeseries/gbd_manual_points_2010_ard_phenology_extracted_Jul30.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-InclProb)

#2009 points
gtrain2 <- st_read("data/training_timeseries/gbd_manual_points_2009_ard_phenology_extracted_Aug28.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb)

#2008 points
gtrain3 <- st_read("data/training_timeseries/gbd_manual_points_2008_ard_phenology_extracted_Aug28.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb)

#2007 points
gtrain4 <- st_read("data/training_timeseries/gbd_manual_points_2007_ard_phenology_extracted_Aug28.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb)

#2006 points
gtrain5 <- st_read("data/training_timeseries/gbd_manual_points_2006_ard_phenology_extracted_Aug6.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb)

#combine 2010 and 2006 points
gtrain <- rbind(gtrain1, gtrain2, gtrain3, gtrain4, gtrain5)

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
#### 4.1.4: landsat ARD extent training data with differenced veg indices (already labelled) ####
gtrain <- gtrain %>% 
  # dplyr::mutate(total_shrubs = SagebrushC) %>% 
  dplyr::select(#sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, ndvi, evi, savi, sr, greenness, 
                #brightness, wetness, 
                #total_shrubs, 
                elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, 
                #precip_anomaly, 
                diff_evi, diff_ndsvi, diff_savi, diff_ndvi, diff_satvi, diff_sr,
                Label) %>%
  dplyr::mutate(#ndsvi = get_ndsvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5),
                #satvi = get_satvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5, band7 = gtrain$sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = Label
                #binary = as.factor(ifelse(total_shrubs < parameters$sc, "Grass", "Shrub"))
  ) %>%
  dplyr::select(#-total_shrubs, 
    - aspect,
    -Label)

#### 4.2: Random Forest Model Training ####

model2 <- randomForest(binary ~ . ,
                      data = gtrain, 
                      ntree = 5000, 
                      mtry = 10, 
                      nodesize = 4,
                      na.action = na.exclude
)
#### END ####

