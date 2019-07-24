# Title: Differenced Spring/Summer NDVI & Other Veg. Indices
# Author(s): Dylan Murphy
# Date Created: 7/22/19
# Date Last Modified: 7/22/19

#### 1. Set Up ####
#Load Packages
libs <- c("raster", "tidyverse", "foreach", "doParallel", "stringr")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Source Scripts (Functions)

source("scripts/functions.R")

#Path Objects
dir.create("data")
dir.create("data/diff_ndvi")

mean_comp_s3 <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites"
mean_comp_local <- "data/mean_composites"

#Pull data from AWS s3 bucket

system(paste0("aws s3 sync ", mean_comp_s3, " ", mean_comp_local))

#create "years" and "mean composites" iterators for looping

mean_composites <- list.files("data/mean_composites", full.names = T)

years <- c()
for(i in 1:length(mean_composites)) {
  years[i] <- substr(mean_composites[i], 25, 28)
}
years <- unique(years)

#### 2. Create Differenced spring/summer NDVI rasters ####
diff_ndvi_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndvi/"

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_ndvi <- get_ndvi(band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_ndvi[spr_ndvi > 1 | spr_ndvi < -1] <- NA
  smr_ndvi <- get_ndvi(band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_ndvi[smr_ndvi > 1 | smr_ndvi < -1] <- NA
  
  diff_ndvi = spr_ndvi - smr_ndvi
  
  
  filename <- paste0("data/diff_ndvi/diff_ndvi_", years[i], ".tif")
  filename_short <- paste0("diff_ndvi_", years[i], ".tif")
  
  writeRaster(diff_ndvi, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_ndvi_destination, filename_short))
  
  rm(spring, summer, spr_ndvi, smr_ndvi, diff_ndvi)
  gc()
}

#### 3. OTHER DIFFERENCED VEG INDICES: ####

#### DIFFERENCED SAVI ####
diff_savi_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/savi/"
dir.create("data/diff_savi")

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_savi <- get_savi(band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_savi[spr_savi > 1 | spr_savi < -1] <- NA
  smr_savi <- get_savi(band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_savi[smr_savi > 1 | smr_savi < -1] <- NA
  
  diff_savi = spr_savi - smr_savi
  
  filename <- paste0("data/diff_savi/diff_savi_", years[i], ".tif")
  filename_short <- paste0("diff_savi_", years[i], ".tif")
  
  writeRaster(diff_savi, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_savi_destination, filename_short))
  
  rm(spring, summer, spr_savi, smr_savi, diff_savi)
  gc()
}


#### DIFFERENCED SR ####
diff_sr_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/sr/"
dir.create("data/diff_sr")

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_sr <- get_sr(band3 = spring$sr_band3, band4 = spring$sr_band4)
  smr_sr <- get_sr(band3 = summer$sr_band3, band4 = summer$sr_band4)
  
  diff_sr = spr_sr - smr_sr
  
  filename <- paste0("data/diff_sr/diff_sr_", years[i], ".tif")
  filename_short <- paste0("diff_sr_", years[i], ".tif")
  
  writeRaster(diff_sr, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_sr_destination, filename_short))
  
  rm(spring, summer, spr_sr, smr_sr, diff_sr)
  gc()
}
#### DIFFERENCED NDSVI ####

diff_ndsvi_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndsvi/"
dir.create("data/diff_ndsvi")

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_ndsvi <- get_ndsvi(band3 = spring$sr_band3, band5 = spring$sr_band5)
  spr_ndsvi[spr_ndsvi > 1 | spr_ndsvi < -1] <- NA
  smr_ndsvi <- get_ndsvi(band3 = summer$sr_band3, band5 = summer$sr_band5)
  smr_ndsvi[smr_ndsvi > 1 | smr_ndsvi < -1] <- NA
  
  diff_ndsvi = spr_ndsvi - smr_ndsvi
  
  filename <- paste0("data/diff_ndsvi/diff_ndsvi_", years[i], ".tif")
  filename_short <- paste0("diff_ndsvi_", years[i], ".tif")
  
  writeRaster(diff_ndsvi, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_ndsvi_destination, filename_short))
  
  rm(spring, summer, spr_ndsvi, smr_ndsvi, diff_ndsvi)
  gc()
}




#### DIFFERENCED EVI ####
diff_evi_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/evi/"
dir.create("data/diff_evi")

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_evi <- get_evi(band1 = spring$sr_band1, band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_evi[spr_evi > 2 | spr_evi < -2] <- NA
  smr_evi <- get_evi(band1 = summer$sr_band1, band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_evi[smr_evi > 2 | smr_evi < -2] <- NA
  
  diff_evi = spr_evi - smr_evi
  
  filename <- paste0("data/diff_evi/diff_evi_", years[i], ".tif")
  filename_short <- paste0("diff_evi_", years[i], ".tif")
  
  writeRaster(diff_evi, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_evi_destination, filename_short))
  
  rm(spring, summer, spr_evi, smr_evi, diff_evi)
  gc()
}



#### DIFFERENCED SATVI ####
diff_satvi_destination <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/satvi/"
dir.create("data/diff_satvi")

for(i in 1:length(years)) {
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  spr_satvi <- get_satvi(band3 = spring$sr_band3, band5 = spring$sr_band5, band7 = spring$sr_band7, L = .5)
  smr_satvi <- get_satvi(band3 = summer$sr_band3, band5 = summer$sr_band5, band7 = summer$sr_band7, L = .5)
  
  diff_satvi = spr_satvi - smr_satvi
  
  filename <- paste0("data/diff_satvi/diff_satvi_", years[i], ".tif")
  filename_short <- paste0("diff_satvi_", years[i], ".tif")
  
  writeRaster(diff_satvi, filename)
  
  system(paste0("aws s3 cp ", filename, " ", diff_satvi_destination, filename_short))
  
  rm(spring, summer, spr_satvi, smr_satvi, diff_satvi)
  gc()
}

