#### Load Packages
libs <- c("randomForest", "tidyverse","sf", "foreach", "doParallel",
          "caTools", # for sample.split()
          "caret", # for confusionMatrix
          "ranger",
          "gganimate",
          "raster",
          #"meteo", #for tiling
          "spdep", #for the weights
          "rfUtilities" # for multi.collinear()
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
