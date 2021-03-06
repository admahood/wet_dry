# Title: Differenced Spring/Summer NDVI & Other Veg. Indices
# Author(s): Dylan Murphy
# Date Created: 7/22/19
# Date Last Modified: 1/13/20

#### 1. Set Up ####
#Load Packages
libs <- c("raster", "tidyverse", "foreach", "doParallel", "stringr")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Source Scripts (Functions)

source("scripts/functions.R")

#Path Objects
dir.create("data")

mean_comp_s3 <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites_ndvi"
mean_comp_local <- "data/mean_composites_done"

#Pull data from AWS s3 bucket

system(paste0("aws s3 sync ", mean_comp_s3, " ", mean_comp_local))

#create sensor platform identifier object for filenames and grabbing proper mean comps

  #IMPORTANT: switch b/w ls5, ls7, and ls5_and_ls7
sensor_platform <- "ls5_and_ls7"
  
#create "years" and "mean composites" iterators for looping

#list mean composite folders (ls5, ls7, ls5_and_ls7)
mean_composite_folders <- list.files("data/mean_composites_done", full.names = T)

#subset to folders containing sensor platform character string
mean_composite_folder <- str_subset(mean_composite_folders, pattern = fixed(sensor_platform))

#b/c ls5_and_ls7 will be grabbed when subsetting for "ls5" or "ls7", second subset here
for(i in 1:length(mean_composite_folder)) {
  if(substr(mean_composite_folder[i], 27, 40) != sensor_platform) {
    mean_composite_folder <- mean_composite_folder[-i]
  } 
}

#list mean composite files present in selected folder
mean_composites <- list.files(mean_composite_folder, full.names = T)   

#create empty vector to store years  
years <- c()

#loop over each file and grab year 

if(sensor_platform == "ls5_and_ls7") {
  #LS5 & LS7 combined composites
  for(i in 1:length(mean_composites)) {
    years[i] <- substr(mean_composites[i], 42, 45)
  } 
} else {
  #LS5 or LS7
for(i in 1:length(mean_composites)) {
  years[i] <- substr(mean_composites[i], 34, 37)
}
}

#simplify list to unique years present
years <- unique(years)

#### 2. Create Differenced spring/summer NDVI rasters ####

#create destination path for s3 upload at end of loop 
diff_ndvi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndvi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_ndvi")
dir.create(paste0("data/diff_ndvi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_ndvi <- get_ndvi(band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_ndvi[spr_ndvi > 1 | spr_ndvi < -1] <- NA
  smr_ndvi <- get_ndvi(band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_ndvi[smr_ndvi > 1 | smr_ndvi < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_ndvi = spr_ndvi - smr_ndvi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_ndvi/", sensor_platform, "/diff_ndvi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_ndvi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_ndvi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_ndvi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_ndvi, smr_ndvi, diff_ndvi)
  gc()
}

#### 3. OTHER DIFFERENCED VEG INDICES: ####

#### DIFFERENCED SAVI ####

#create destination path for s3 upload at end of loop 
diff_savi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/savi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_savi")
dir.create(paste0("data/diff_savi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_savi <- get_savi(band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_savi[spr_savi > 1 | spr_savi < -1] <- NA
  smr_savi <- get_savi(band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_savi[smr_savi > 1 | smr_savi < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_savi = spr_savi - smr_savi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_savi/", sensor_platform, "/diff_savi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_savi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_savi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_savi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_savi, smr_savi, diff_savi)
  gc()
}


#### DIFFERENCED SR ####

#create destination path for s3 upload at end of loop 
diff_sr_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/sr/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_sr")
dir.create(paste0("data/diff_sr/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season 
  spr_sr <- get_sr(band3 = spring$sr_band3, band4 = spring$sr_band4)
  smr_sr <- get_sr(band3 = summer$sr_band3, band4 = summer$sr_band4)
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_sr = spr_sr - smr_sr
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_sr/", sensor_platform, "/diff_sr_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_sr_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_sr, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_sr_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_sr, smr_sr, diff_sr)
  gc()
}
#### DIFFERENCED NDSVI ####

#create destination path for s3 upload at end of loop 
diff_ndsvi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndsvi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_ndsvi")
dir.create(paste0("data/diff_ndsvi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_ndsvi <- get_ndsvi(band3 = spring$sr_band3, band5 = spring$sr_band5)
  spr_ndsvi[spr_ndsvi > 1 | spr_ndsvi < -1] <- NA
  smr_ndsvi <- get_ndsvi(band3 = summer$sr_band3, band5 = summer$sr_band5)
  smr_ndsvi[smr_ndsvi > 1 | smr_ndsvi < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_ndsvi = spr_ndsvi - smr_ndsvi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_ndsvi/", sensor_platform, "/diff_ndsvi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_ndsvi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_ndsvi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_ndsvi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_ndsvi, smr_ndsvi, diff_ndsvi)
  gc()
}




#### DIFFERENCED EVI ####

#create destination path for s3 upload at end of loop 
diff_evi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/evi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_evi")
dir.create(paste0("data/diff_evi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_evi <- get_evi(band1 = spring$sr_band1, band3 = spring$sr_band3, band4 = spring$sr_band4)
  spr_evi[spr_evi > 2 | spr_evi < -2] <- NA
  smr_evi <- get_evi(band1 = summer$sr_band1, band3 = summer$sr_band3, band4 = summer$sr_band4)
  smr_evi[smr_evi > 2 | smr_evi < -2] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_evi = spr_evi - smr_evi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_evi/", sensor_platform, "/diff_evi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_evi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_evi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_evi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_evi, smr_evi, diff_evi)
  gc()
}



#### DIFFERENCED SATVI ####

#create destination path for s3 upload at end of loop 
diff_satvi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/satvi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_satvi")
dir.create(paste0("data/diff_satvi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_satvi <- get_satvi(band3 = spring$sr_band3, band5 = spring$sr_band5, band7 = spring$sr_band7, L = .5)
  smr_satvi <- get_satvi(band3 = summer$sr_band3, band5 = summer$sr_band5, band7 = summer$sr_band7, L = .5)
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_satvi = spr_satvi - smr_satvi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_satvi/", sensor_platform, "/diff_satvi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_satvi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally 
  writeRaster(diff_satvi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_satvi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_satvi, smr_satvi, diff_satvi)
  gc()
}


#### DIFFERENCED NDTI #### 
#create destination path for s3 upload at end of loop 
diff_ndti_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndti/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_ndti")
dir.create(paste0("data/diff_ndti/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_ndti <- get_ndti(band5 = spring$sr_band5, band7 = spring$sr_band7)
  spr_ndti[spr_ndti > 1 | spr_ndti < -1] <- NA
  smr_ndti <- get_ndti(band5 = summer$sr_band5, band7 = summer$sr_band7)
  smr_ndti[smr_ndti > 1 | smr_ndti < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_ndti = spr_ndti - smr_ndti
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_ndti/", sensor_platform, "/diff_ndti_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_ndti_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_ndti, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_ndti_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_ndti, smr_ndti, diff_ndti)
  gc()
}

#### DIFFERENCED GREEN NDVI ####
#create destination path for s3 upload at end of loop 
diff_green_ndvi_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/green_ndvi/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_green_ndvi")
dir.create(paste0("data/diff_green_ndvi/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_green_ndvi <- get_green_ndvi(band4 = spring$sr_band4, band2 = spring$sr_band2)
  spr_green_ndvi[spr_green_ndvi > 1 | spr_green_ndvi < -1] <- NA
  smr_green_ndvi <- get_green_ndvi(band4 = summer$sr_band4, band2 = summer$sr_band2)
  smr_green_ndvi[smr_green_ndvi > 1 | smr_green_ndvi < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_green_ndvi = spr_green_ndvi - smr_green_ndvi
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_green_ndvi/", sensor_platform, "/diff_green_ndvi_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_green_ndvi_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_green_ndvi, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_green_ndvi_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_green_ndvi, smr_green_ndvi, diff_green_ndvi)
  gc()
}

#### DIFFERENCED SLA INDEX ####
#create destination path for s3 upload at end of loop 
diff_sla_index_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/sla_index/", sensor_platform, "/")
#create local directory to write rasters to within loop 
dir.create("data/diff_sla_index")
dir.create(paste0("data/diff_sla_index/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_sla_index <- get_SLA_index(band3 = spring$sr_band3, band4 = spring$sr_band4, band7 = spring$sr_band7)
  #spr_sla_index[spr_sla_index > 1 | spr_sla_index < -1] <- NA
  smr_sla_index <- get_SLA_index(band3 = summer$sr_band3, band4 = summer$sr_band4, band7 = summer$sr_band7)
  #smr_sla_index[smr_sla_index > 1 | smr_sla_index < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_sla_index = spr_sla_index - smr_sla_index
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_sla_index/", sensor_platform, "/diff_sla_index_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_sla_index_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_sla_index, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_sla_index_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_sla_index, smr_sla_index, diff_sla_index)
  gc()
}

#### DIFFERENCED NDI7 ####
#create destination path for s3 upload at end of loop 
diff_ndi7_destination <- paste0("s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/ndi_7/", sensor_platform, "/")

#create local directory to write rasters to within loop 
dir.create("data/diff_ndi7")
dir.create(paste0("data/diff_ndi7/", sensor_platform))

for(i in 1:length(years)) {
  
  #grab *SPRING* landsat ARD mean composite that matches the year you are iterating
  spring <- str_subset(mean_composites, pattern = fixed("spring"))
  spring <- stack(str_subset(spring, pattern = fixed(years[i])))
  names(spring) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #grab *SUMMER* landsat ARD mean composite that matches the year you are iterating
  summer <- str_subset(mean_composites, pattern = fixed("summer"))
  summer <- stack(str_subset(summer, pattern = fixed(years[i])))
  names(summer) <- c("sr_band1","sr_band2","sr_band3","sr_band4","sr_band5","sr_band7")
  
  #Calculate Veg Index of interest for each season & remove anomalous outlier values (outside of expected range)
  spr_ndi7 <- get_ndi7(band4 = spring$sr_band4, band7 = spring$sr_band7)
  spr_ndi7[spr_ndi7 > 1 | spr_ndi7 < -1] <- NA
  smr_ndi7 <- get_ndi7(band4 = summer$sr_band4, band7 = summer$sr_band7)
  smr_ndi7[smr_ndi7 > 1 | smr_ndi7 < -1] <- NA
  
  #Subtract summer veg index values from spring values; create "differenced" veg index raster
  diff_ndi7 = spr_ndi7 - smr_ndi7
  
  #create unique filename for differenced raster using veg index and year (later will add tile location to this)
  filename <- paste0("data/diff_ndi7/", sensor_platform, "/diff_ndi7_", years[i], "_", sensor_platform, ".tif")
  filename_short <- paste0("diff_ndi7_", years[i], "_", sensor_platform, ".tif")
  
  #save raster locally
  writeRaster(diff_ndi7, filename)
  
  #upload differenced veg index raster to s3 
  system(paste0("aws s3 cp ", filename, " ", diff_ndi7_destination, filename_short))
  
  #remove raster objects from memory to keep memory usage down
  rm(spring, summer, spr_ndi7, smr_ndi7, diff_ndi7)
  gc()
}
