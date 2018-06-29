# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")

# paths -----------------------------------
dir.create("data/results")
ls5_list <- list.files(local_path, full.names = T) 
ls5_files <- list.files(local_path)

s3_path <- "s3://earthlab-amahood/data/ls5_pixel_replaced_crop_test"
local_path <- "data/terrain_crop_test"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_2"

s3_result <- "s3://earthlab-amahood/data/model_applied_scenes"

#s3 sync -------------------------------- 
system(paste0("aws s3 sync ", s3_path, " ", local_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#cropping and stacking and applying the model loop --------------------------

ter <- stack(list.files(local_terrain, full.names =T))

for(i in 1:length(ls5_list)) {
  t0 <- Sys.time()
  ls5 <- stack(ls5_list[i])
  names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
  filenamet <- paste0("home/rstudio/wet_dry/data/results/",  
                      sub(pattern = ".tif", 
                          replacement = "model_results.tif", 
                          x = ls5_files[i], fixed = T))
  ter_c <-raster::resample(ter,ls5)
  
  # now, make sure all the names of this stack match the names that go into the model and we're golden
  ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
  ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
  ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
  ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
  
  ls5 <- stack(ls5, ter_c)
  print(Sys.time()-t0)
  
  # now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file)
  
  #writeRaster(rasterwithmodelapplied, filename = filenamet, overwrite = T)
  #system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/.....))
  #unlink(filenamet)
}

#non looping attempt ------------ 
# if you want to test a loop, just type i=1 in the console (or whatever your iterator is)
# and move through it that way, that way you don't have a bunch of repeated code floating around
