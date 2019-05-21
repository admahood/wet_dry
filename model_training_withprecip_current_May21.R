#Script Title: randomForest model training with precipitation anomaly data & two methods of class labelling
#Date Last Edited: 5/21/19
#Author(s): Dylan Murphy

#### 1: Load Packages/Source scripts/set seed

libs <- c("randomForest", "dplyr","sf", "caTools")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")
set.seed(11)

#### 2: Pull data from s3: Hypergrids (for parameter selection) & Training Data (csv)

system("aws s3 sync s3://earthlab-amahood/data/hypergrids_vb data/hypergrids") # Use Oct 16 hg (until new hg run is complete)
system("aws s3 sync s3://earthlab-amahood/data/data_splits data/data_splits") # Use May 21 splits (most recent)

#### 3: Hypergrid parameterization: For now, I am using the parameters used for previous model runs (from best_randomforest_list_script.R)
####    the hypergrids will need to be run again to find optimal parameters for models using new training/test data

hypergrid <- read.csv("data/hypergrids/hgOct_16_2018.csv") %>% arrange(desc(balanced_accuracy))

best_hyper <- hypergrid[c(1:5, 7, 10, 13, 16, 17, 19, 20, 41, 49, 51),] %>% arrange(desc(balanced_accuracy))

ensemble_parameters <- best_hyper[c(12, 8, 11),]

parameters <- ensemble_parameters[1,] # selecting just the balanced model for now

#### 4: Model Training - Just for one model right now (sc = 14), but can be changed to create model ensemble group

#### 4.1: Create Training Data Class Labels - needs to be refined going forward 

gtrain <- read.csv("data/data_splits/gtrain_May21_2019.csv") 

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
  dplyr::select(-total_shrubs)

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
shrubs<- dplyr::filter(gtrain, SagebrushC > 0 & InvAnnGras < 4) %>%
  mutate(cluster = 2);dim(shrubs)

grasses <- dplyr::filter(gtrain, SagebrushC <2 & InvAnnGras >4)%>%
  mutate(cluster= 1);dim(grasses)

gtrain_new <- rbind(grasses,shrubs)
gtrain <- gtrain_new 
gtrain <- gtrain %>% 
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7, ndvi, evi, savi, sr, greenness, 
                brightness, wetness, elevation, slope, aspect, 
                tpi, tri, roughness, flowdir, cluster, precip_anomaly) %>%
  dplyr::mutate(ndsvi = get_ndsvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5),
                satvi = get_satvi(band3 = gtrain$sr_band3, band5 = gtrain$sr_band5, band7 = gtrain$sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                binary = gtrain$cluster) %>%
  dplyr::select(-cluster)

#### 4.2: Random Forest Model Training ####

model <- randomForest(binary ~ . ,
                      data = gtrain, 
                      ntree = 2000, 
                      mtry = parameters$mtry, 
                      nodesize = parameters$nodesize 
)
#### END ####

