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

#### Naip scene for trimming
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif")
# naip<-list()
# naip[[1]] <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc
# naip[[2]] <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") #frank
# naip[[3]] <- raster("data/naip/m_4111823_sw_11_1_20100628.tif")

#### ls5 scene for projection/resolution matching while downsampling
ls5 <- stack(list.files("data/ls5", full.names = T))

#reprojecting, cropping, resampling PRISM annual precip to 30m resolution 
prism <- list()
folders <- list.files("data/precip_annual/PRISM_annual_precip", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- projectRaster(prism_oneyr, ls5, res = 30)
  prism_oneyr <- crop(prism_oneyr, naip)
  prism[i] <- prism_oneyr
}

#setup PRISM 30yr normals raster (reproject, resample, crop to NAIP scene)
normals <- raster("data/precip_annual/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
normals <- projectRaster(normals, ls5, res = 30)
normals <- crop(normals, naip)

#create years iterator for file names
years <- (1984:2011)

#create directory to store precip anomaly rasters
dir.create("data/precip_annual/naip_trimmed_anomaly")

#loop over each annual precip raster and create precip anomaly rasters (annual deviation from 30yr normals)
for (i in 1:length(prism)) {
  filename <- paste0("data/precip_annual/naip_trimmed_anomaly/precip_anomaly_trimmed_", years[i], ".tif")
  filename_short <- paste0("precip_anomaly_trimmed_", years[i], ".tif")
  annual_precip_anomaly <- prism[[i]] - normals
  writeRaster(annual_precip_anomaly, filename = filename)
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/PRISM_precip_annual/naip_trimmed_annual_precip_anomaly/", filename_short))
}

#### TRAINING DATA PRECIP ANOMALY EXTRACTION ####
#load in training data to attach precip anomaly
#system("aws s3 sync s3://earthlab-amahood/data/data_splits data/data_splits")
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")

#load normals raster & grab crs
normals <- raster("data/precip_annual/PRISM_30yr_precip_normals/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
crs <- crs(normals)

#load in ecoregions, merge to Gr. Basin shapefile, set as a spatial object for crop (not sf object)
ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp")
great_basin <- subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain"))
great_basin <- st_union(great_basin)
great_basin <- st_transform(great_basin, crs = crs)
great_basin <- as(great_basin, "Spatial")

#crop normals to great basin extent
normals_gb <- crop(normals, great_basin)


#create annual precipitation rasters for training years (2011-2015)
trainprism <- list()
folders <- list.files("data/precip_annual/PRISM_annual_precip_training", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  #prism_oneyr <- projectRaster(prism_oneyr, crs = crs, res = 30)
  prism_oneyr <- crop(prism_oneyr, great_basin)
  trainprism[i] <- prism_oneyr
}

#create directory for storing training years precip anomaly rasters
dir.create("data/precip_annual/greatbasin_trimmed_anomaly_training")
#make vector of training years for filenames
train_years <- (2011:2015)
#loop over each year's precipitation raster, create precip anomaly raster, save and upload to s3
for (i in 1:length(trainprism)) {
  filename <- paste0("data/precip_annual/greatbasin_trimmed_anomaly_training/precip_anomaly_train_", train_years[i], ".tif")
  filename_short <- paste0("precip_anomaly_train", train_years[i], ".tif")
  annual_precip_anomaly <- trainprism[[i]] - normals_gb
  writeRaster(annual_precip_anomaly, filename = filename)
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/PRISM_precip_annual/greatbasin_trimmed_anomaly_training/", filename_short))
}

#load training data points
gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) 

#extract normals to gb training data
gbd <- dplyr::mutate(gbd, normals = raster::extract(normals_gb, gbd))



