# PRISM PRECIPITATION ANOMALY RASTERS & EXTRACTION TO TRAINING DATA POINTS SCRIPT
# Author: Dylan Murphy
# Date Last Modified: 12/6/19

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
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/naip data/naip")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/PRISM_precip data/precip")
system("aws s3 cp s3://earthlab-amahood/wet_dry/input_raster_data/ls5_mucc/ls5_1984_042031_.tif data/ls5/ls5_1984_042031_.tif")

#### 2: creating NAIP trimmed precip anomaly rasters for model input ####

#### Naip scene for trimming
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc scene - switch path to one from below to make new naip crops
# naip<-list()
# naip[[1]] <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc
# naip[[2]] <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") #frank
# naip[[3]] <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") #kings

#### ls5 scene for projection/resolution matching while downsampling
ls5 <- stack(list.files("data/ls5", full.names = T))

#### reprojecting, cropping, resampling PRISM annual precip to 30m resolution 
prism <- list()
folders <- list.files("data/precip/PRISM_annual_precip", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- projectRaster(prism_oneyr, ls5, res = 30)
  prism_oneyr <- crop(prism_oneyr, naip)
  prism[i] <- prism_oneyr
  print("oneyr done")
}

#### setup PRISM 30yr normals raster (reproject, resample, crop to NAIP scene)
normals <- raster("data/precip/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
normals <- projectRaster(normals, ls5, res = 30)
normals <- crop(normals, naip)

#### create years iterator for file names
years <- (1984:2011)

#### create directory to store precip anomaly rasters
dir.create("data/precip/naip_trimmed_anomaly")

#### loop over each annual precip raster and create precip anomaly rasters (annual deviation from 30yr normals)
for (i in 1:length(prism)) {
  #filenames - change "frank"/"wmuc"/"kings" to name file based on which naip was used
  filename <- paste0("data/precip/naip_trimmed_anomaly/precip_anomaly_trimmed_kings_", years[i], ".tif")
  filename_short <- paste0("precip_anomaly_trimmed_kings_", years[i], ".tif")
  
  annual_precip_anomaly <- prism[[i]] - normals
  writeRaster(annual_precip_anomaly, filename = filename)
  #change "frank"/"wmuc"/"kings" to the desired folder on s3 based on which naip was used below
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/wet_dry/PRISM_precip/naip_trimmed_annual_precip_anomaly/", "kings/", filename_short))
}

gc()

#### 3: Creating Gr. Basin Normals Raster####

#### load normals raster & grab crs
normals <- raster("data/precip/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
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

#### crop normals to great basin extent
normals_gb <- crop(normals, great_basin)

####reproject normals to match ls5 resolution and crs
normals_gb <- projectRaster(normals_gb, crs = crs2, res = 30)

####saving and uploading cropped and reprojected normals raster to s3
writeRaster(normals_gb, filename = "data/precip/gb_cropped_normals.tif")
system("aws s3 cp data/precip/gb_cropped_normals.tif s3://earthlab-amahood/data/PRISM_precip/gb_cropped_normals.tif")

#### 4: Resampling MONTHLY actual precip data for model input ####

#create directories to store resampled/cropped precip rasters
dir.create("data/precip/monthly_resampled_cropped")
dir.create("data/precip/monthly_resampled_cropped/wmuc")
dir.create("data/precip/monthly_resampled_cropped/kings")
dir.create("data/precip/monthly_resampled_cropped/frank")

#### ls5 scene for projection/resolution matching while downsampling
ls5 <- stack(list.files("data/ls5", full.names = T))

#### Naip scene for trimming
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc scene - switch path to one from below to make new naip crops

#other naip scene paths
# "data/naip/m_4011703_ne_11_1_20100704.tif" #wmuc
# "data/naip/n_4111761_nw_11_1_20060813.tif" #frank
# "data/naip/m_4111823_sw_11_1_20100628.tif" #kings

#### ls5 scene for projection/resolution matching while downsampling
ls5 <- stack(list.files("data/ls5", full.names = T))

#create naip name object for filename creation (change this depending on which scene you are using) 
naip_name <- "wmuc" #"kings" #"frank"

#grab folder filenames (each folder is one year of monthly precip rasters)
folders <- list.files("data/precip/PRISM_monthly_precip", full.names = T)

#### reprojecting, cropping, resampling PRISM monthly precip to 30m resolution 
for (i in 1:length(folders)) {
  prism_oneyr <- list.files(folders[i], pattern = "\\.bil$", full.names = T)
  prism_oneyr <- prism_oneyr[-1]
  for(j in 1:length(prism_oneyr)) {
  filename <- paste0("data/precip/monthly_resampled_cropped/", naip_name, "/precip_", substr(prism_oneyr[j], 93, 98), ".tif")
  prism_1month <- raster(prism_oneyr[j])
  prism_1month <- projectRaster(prism_1month, ls5, res = 30)
  print(paste0(names(prism_1month), " resampled"))
  prism_1month <- crop(prism_1month, naip)
  print(paste0(names(prism_1month), " cropped"))
  writeRaster(prism_1month, filename)
  print(paste0(names(prism_1month), " done"))
}
}

system("aws s3 sync data/precip/monthly_resampled_cropped s3://earthlab-amahood/wet_dry/input_raster_data/PRISM_precip/monthly_precip_resample/cropped")
#### 5: ( All steps starting wth "5" ARE NO LONGER USED but will be useful if we ever need to go back to blm aim data ####
####TRAINING DATA (2011-2015) PRECIP ANOMALY EXTRACTION ####
#load in training data to attach precip anomaly
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")
#### 5.2: IF NORMALS RASTER IS DONE, START HERE ####
normals_gb <- raster("data/precip/gb_cropped_normals.tif")
#### 5.3: create annual precipitation rasters for training years (2011-2015) - IF THESE ARE DONE SKIP TO NEXT SECTION ####
trainprism <- list()
folders <- list.files("data/precip/PRISM_annual_precip_training", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- crop(prism_oneyr, great_basin)
  prism_oneyr <- projectRaster(prism_oneyr, crs = crs2, res = 30)
  trainprism[i] <- prism_oneyr
}

#### create directory for storing training years precip anomaly rasters
dir.create("data/precip/greatbasin_trimmed_anomaly_training")
#### make vector of training years for filenames
train_years <- (2011:2015)
#### loop over each year's precipitation raster, create precip anomaly raster, save and upload to s3
for (i in 1:length(trainprism)) {
  filename <- paste0("data/precip/greatbasin_trimmed_anomaly_training/precip_anomaly_train_", train_years[i], ".tif")
  filename_short <- paste0("precip_anomaly_train", train_years[i], ".tif")
  annual_precip_anomaly <- trainprism[[i]] - normals_gb
  writeRaster(annual_precip_anomaly, filename = filename)
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/PRISM_precip/greatbasin_trimmed_anomaly_training/", filename_short))
}

#### 5.4: Extracting precip data to blm data points - IF NORMALS AND ANOMALY RASTERS ARE DONE START HERE ####

####load training data points

system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) 

#### extract normals to gb training data

gbd <- dplyr::mutate(gbd, normals = raster::extract(normals_gb, gbd))

####create objects for full great basin precip anomaly rasters
training_anomaly_paths <- list.files("data/precip/greatbasin_trimmed_anomaly_training", full.names = T)
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
system("aws s3 cp data/gbd_plots_w_precip_5_20_19.csv s3://earthlab-amahood/data/PRISM_precip/gbd_plots_w_precip_5_20_19.csv")
