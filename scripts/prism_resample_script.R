# PRISM PRECIPITATION ANOMALY RASTERS & EXTRACTION TO TRAINING DATA POINTS SCRIPT
# Author: Dylan Murphy
# Date Last Modified: 5/23/19

#### 1: Set up ####
#### Load Packages
libs <- c("randomForest", "tidyverse","sf", "foreach", "doParallel",
          "caTools", # for sample.split()
          "caret", # for confusionMatrix
          "ranger",
          "gganimate",
          "raster",
          #"meteo", #for tiling
          "spdep", #for the weights
          "rfUtilities", # for multi.collinear()
          "dplyr",
          "rgdal"
          )

#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)


#### Pull data from AWS
system("aws s3 sync s3://earthlab-amahood/data/naip data/naip")
system("aws s3 sync s3://earthlab-amahood/data/PRISM_precip_annual data/precip_annual")
system("aws s3 sync s3://earthlab-amahood/data/ls5_mucc_2011 data/ls5")

#### 2: creating NAIP trimmed precip anomaly rasters for model input ####

#### Naip scene for trimming
naip <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") #kings scene - switch path to one from below to make new naip crops
# naip<-list()
# naip[[1]] <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc
# naip[[2]] <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") #frank
# naip[[3]] <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") #kings

#### ls5 scene for projection/resolution matching while downsampling
ls5 <- stack(list.files("data/ls5", full.names = T))

#### reprojecting, cropping, resampling PRISM annual precip to 30m resolution 
prism <- list()
folders <- list.files("data/precip_annual/PRISM_annual_precip", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- projectRaster(prism_oneyr, ls5, res = 30)
  prism_oneyr <- crop(prism_oneyr, naip)
  prism[i] <- prism_oneyr
  print("oneyr done")
}

#### setup PRISM 30yr normals raster (reproject, resample, crop to NAIP scene)
normals <- raster("data/precip_annual/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
normals <- projectRaster(normals, ls5, res = 30)
normals <- crop(normals, naip)

#### create years iterator for file names
years <- (1984:2011)

#### create directory to store precip anomaly rasters
dir.create("data/precip_annual/naip_trimmed_anomaly")

#### loop over each annual precip raster and create precip anomaly rasters (annual deviation from 30yr normals)
for (i in 1:length(prism)) {
  #filenames - change "frank"/"wmuc"/"kings" to name file based on which naip was used
  filename <- paste0("data/precip_annual/naip_trimmed_anomaly/precip_anomaly_trimmed_kings_", years[i], ".tif")
  filename_short <- paste0("precip_anomaly_trimmed_kings_", years[i], ".tif")
  
  annual_precip_anomaly <- prism[[i]] - normals
  writeRaster(annual_precip_anomaly, filename = filename)
  #change "frank"/"wmuc"/"kings" to the desired folder on s3 based on which naip was used below
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/PRISM_precip_annual/naip_trimmed_annual_precip_anomaly/", "kings/", filename_short))
}

gc()
#### 3: TRAINING DATA (2011-2015) PRECIP ANOMALY EXTRACTION ####
#load in training data to attach precip anomaly
#system("aws s3 sync s3://earthlab-amahood/data/data_splits data/data_splits")
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")

#### load normals raster & grab crs
normals <- raster("data/precip_annual/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
ls5 <- stack(list.files("data/ls5", full.names = T))
crs1 <- crs(normals)
crs2 <- crs(ls5)

#### load in ecoregions, merge to Gr. Basin shapefile, set as a spatial object for crop (not sf object)
ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp")
great_basin <- subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain"))
great_basin <- st_union(great_basin)
great_basin <- st_transform(great_basin, crs = crs1)
great_basin <- as(great_basin, "Spatial")
#### 3.1: Creating Gr. Basin Normals Raster - IF THIS STEP IS DONE SKIP TO BELOW THIS SECTION####
#### crop normals to great basin extent
normals_gb <- crop(normals, great_basin)

####reproject normals to match ls5 resolution and crs
normals_gb <- projectRaster(normals_gb, crs = crs2, res = 30)

####saving and uploading cropped and reprojected normals raster to s3
writeRaster(normals_gb, filename = "data/precip_annual/gb_cropped_normals.tif")
system("aws s3 cp data/precip_annual/gb_cropped_normals.tif s3://earthlab-amahood/data/PRISM_precip_annual/gb_cropped_normals.tif")

#### 3.2: IF NORMALS RASTER IS DONE, START HERE ####
normals_gb <- raster("data/precip_annual/gb_cropped_normals.tif")
#### 3.3: create annual precipitation rasters for training years (2011-2015) - IF THESE ARE DONE SKIP TO NEXT SECTION ####
trainprism <- list()
folders <- list.files("data/precip_annual/PRISM_annual_precip_training", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- crop(prism_oneyr, great_basin)
  prism_oneyr <- projectRaster(prism_oneyr, crs = crs2, res = 30)
  trainprism[i] <- prism_oneyr
}

#### create directory for storing training years precip anomaly rasters
dir.create("data/precip_annual/greatbasin_trimmed_anomaly_training")
#### make vector of training years for filenames
train_years <- (2011:2015)
#### loop over each year's precipitation raster, create precip anomaly raster, save and upload to s3
for (i in 1:length(trainprism)) {
  filename <- paste0("data/precip_annual/greatbasin_trimmed_anomaly_training/precip_anomaly_train_", train_years[i], ".tif")
  filename_short <- paste0("precip_anomaly_train", train_years[i], ".tif")
  annual_precip_anomaly <- trainprism[[i]] - normals_gb
  writeRaster(annual_precip_anomaly, filename = filename)
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/PRISM_precip_annual/greatbasin_trimmed_anomaly_training/", filename_short))
}

#### 3.4: Extracting precip data to blm data points - IF NORMALS AND ANOMALY RASTERS ARE DONE START HERE ####

####load training data points

system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) 

#### extract normals to gb training data

gbd <- dplyr::mutate(gbd, normals = raster::extract(normals_gb, gbd))

####create objects for full great basin precip anomaly rasters
training_anomaly_paths <- list.files("data/precip_annual/greatbasin_trimmed_anomaly_training", full.names = T)
training_anomaly <- list()
for(i in 1:length(training_anomaly_paths)) {
  training_anomaly[i] <- raster(training_anomaly_paths[i])
}

#### extract precip anomaly to training data points with matching year 
precip_anomaly_vec <- c()
for(i in 1:nrow(gbd)) {
  precip_year <- as.numeric(gbd[i,]$year)
 precip_anomaly_vec[i] <- raster::extract(training_anomaly[[precip_year]], gbd[i,])
}


#### attach annual precip anomaly to gb training data
gbd <- dplyr::mutate(gbd, precip_anomaly = precip_anomaly_vec)

#### save new blm data with precip as a spatially registered gpkg and upload to s3 (use this for future variable extraction)
st_write(gbd, dsn = "data/gbd_plots_w_precip_5_20_19_points.gpkg")
system("aws s3 cp data/gbd_plots_w_precip_5_20_19_points.gpkg s3://earthlab-amahood/data/gbd_plots_w_precip_5_20_19_points.gpkg")

#### save new blm data with precip as a csv and upload to s3
write.csv(gbd, file = "data/gbd_plots_w_precip_5_20_19.csv")
system("aws s3 cp data/gbd_plots_w_precip_5_20_19.csv s3://earthlab-amahood/data/PRISM_precip_annual/gbd_plots_w_precip_5_20_19.csv")

#### 4: training/test data splitting & saving to s3 for reuse (5/21/19) ####
# **THESE ARE THE MOST RECENT TRAINING/TEST DATA**

# create split variable (70% training, 30% test/dev)
gbd1 <-  st_set_geometry(gbd,NULL)%>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7)) 

#Select Training Data based on split & push as csv to s3
gtrain <- filter(gbd1,split == TRUE) %>% dplyr::select(-split)
write.csv(gtrain, file = "data/gtrain_May21_2019.csv")
system("aws s3 cp data/gtrain_May21_2019.csv s3://earthlab-amahood/data/data_splits/gtrain_May21_2019.csv")

#select test/dev data based on split & split again to get 15% test; 15% dev
gtestdev <- filter(gbd1,split == FALSE) %>% dplyr::select(-split) %>% 
  mutate(
    split = 1,
    split = sample.split(split, SplitRatio=0.5)
    )

#select test data and save to s3 as csv
gtest <- filter(gtestdev,split == TRUE) %>% dplyr::select(-split)
write.csv(gtest, file = "data/gtest_May21_2019.csv")
system("aws s3 cp data/gtest_May21_2019.csv s3://earthlab-amahood/data/data_splits/gtest_May21_2019.csv")
 
#select dev data and save to s3 as csv       
gdev <- filter(gtestdev,split == FALSE) %>% dplyr::select(-split)
write.csv(gdev, file = "data/gdev_May21_2019.csv")
system("aws s3 cp data/gdev_May21_2019.csv s3://earthlab-amahood/data/data_splits/gdev_May21_2019.csv")

#### END ####
