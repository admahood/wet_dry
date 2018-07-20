# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")
source("scripts/updated_random_forest_script.R")
# paths -----------------------------------
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

s3_path <- "s3://earthlab-amahood/data/ls5_mucc"
local_path <- "data/ls5_mucc"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_2"

s3_result <- "s3://earthlab-amahood/data/model_applied_scenes"

#s3 sync -------------------------------- 
system(paste0("aws s3 sync ", s3_path, " ", local_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#cropping and stacking and applying the model loop --------------------------
ls5_list <- list.files(local_path, full.names = T) 
ls5_files <- list.files(local_path)

cores <- length(ls5_list)

registerDoParallel(cores)

foreach(i = ls5_list) %dopar% {
  file <- as.character(i)
  t0 <- Sys.time()
  ter <- stack(list.files(local_terrain, full.names =T))
  ls5 <- stack(i)
  names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
  filenamet <- paste0("data/results/",
                      sub(pattern = ".tif", 
                          replacement = "model_results.tif", 
                          x = substr(file, 15, 34), fixed = T)) 
  
  ter_c <-raster::resample(ter,ls5)
  system(paste("echo", "terrain cropped", i))
  # now, make sure all the names of this stack match the names that go into the model and we're golden
  ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
  ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
  ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
  ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
  ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
  
  ls5 <- stack(ls5, ter_c)
  
  names(ls5) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "wetness", "brightness", "greenness", "ndvi", "savi", "sr", "evi", "aspect", "flowdir", "elevation", "roughness", "slope", 
                  "tpi", "tri") #names to match exactly with training data that goes into model. The order matters for these
  
  system(paste("echo", "stack created", i))
  
  print(Sys.time()-t0)
  gc() # for saving memory
  
  # now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file) 
  ls5_classed <- predict(ls5, forest_1, type = 'class', progress = 'text', inf.rm = T, na.rm = T)

  system(paste("echo", "model applied", i))
  
  system(paste("echo", "", i))
  print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model
  
  writeRaster(ls5_classed, filename = filenamet, overwrite = T, progress = 'text')
  
  system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/ls5_model_results_test_mucc/"))
  system(paste("echo", i, "done"))
  unlink(filenamet)
  system("rm data/tmp/*") # so we're not filling up the hard drive (had to move this to the end because the model needs the stuff stored in temp directory - D)
}

#non looping attempt ------------ 
# if you want to test a loop, just type i=1 in the console (or whatever your iterator is)
# and move through it that way, that way you don't have a bunch of repeated code floating around


# then we create another section that loops through each year, creating big mosaics