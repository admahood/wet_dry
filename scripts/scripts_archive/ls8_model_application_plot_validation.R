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

s3_path <- "s3://earthlab-amahood/data/ls8_validation_0718"
local_path <- "data/ls8_validation"
stack_path <- "data/ls8_validation_stack"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_2"

s3_result <- "s3://earthlab-amahood/data/model_applied_scenes"

#s3 sync -------------------------------- 
system(paste0("aws s3 sync ", s3_path, " ", local_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#untarring and stacking tifs
untar(list.files(local_path), exdir = paste0(local_path, "/", "tifs"))

ls8_tifs <- list.files(paste0(local_path, "/", "tifs"))
ls8_tifs <- ls8_tifs[-1]
                       
ls8_stack <- stack(ls8_tifs)

writeRaster(ls8_stack, paste0(stack_path, "/", "ls8_validation_scene"), format = "GTiff")


#cropping and stacking and applying the model loop --------------------------
ls8_list <- list.files(stack_path, full.names = T) 
ls8_files <- list.files(local_path)
ter <- stack(list.files(local_terrain, full.names =T))


for(i in 1:length(ls8_list)) {
  t0 <- Sys.time()
  ls8 <- stack(ls8_list[i])
  names(ls8)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
  filenamet <- paste0("data/results/",  
                      sub(pattern = ".tif", 
                          replacement = "model_results.tif", 
                          x = substr(ls8_list[i], 26, 50), fixed = T)) 
  
  ter_c <-raster::resample(ter,ls8)
  
  # now, make sure all the names of this stack match the names that go into the model and we're golden
  ls8$wetness <- wet8(ls8$sr_band1,ls8$sr_band2,ls8$sr_band3,ls8$sr_band4,ls8$sr_band5,ls8$sr_band7)
  ls8$brightness <- bright8(ls8$sr_band1,ls8$sr_band2,ls8$sr_band3,ls8$sr_band4,ls8$sr_band5,ls8$sr_band7)
  ls8$greenness <- green8(ls8$sr_band1,ls8$sr_band2,ls8$sr_band3,ls8$sr_band4,ls8$sr_band5,ls8$sr_band7)
  ls8$ndvi <- get_ndvi8(ls8$sr_band3, ls8$sr_band4)
  ls8$savi <- get_savi8(ls8$sr_band3, ls8$sr_band4)
  ls8$sr <- get_sr8(ls8$sr_band3, ls8$sr_band4)
  ls8$evi <- get_evi8(ls8$sr_band1, ls8$sr_band3, ls8$sr_band4)
  
  ls8 <- stack(ls8, ter_c)
  
  names(ls8) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7", "wetness", "brightness", "greenness", "ndvi", "savi", "sr", "evi", "aspect", "flowdir", "elevation", "roughness", "slope", 
                  "tpi", "tri") #names to match exactly with training data that goes into model. The order matters for these
  
  print(Sys.time()-t0)
  gc() # for saving memory
  
  # now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file) 
  ls8_classed <- predict(ls8, frst, type = 'class', progress = 'text', inf.rm = T, na.rm = T)

  print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model
  
  writeRaster(ls8_classed, filename = filenamet, overwrite = T, progress = 'text')
  
  system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/ls8_model_results_test_0718/"))
  unlink(filenamet)
  system("rm data/tmp/*") # so we're not filling up the hard drive (had to move this to the end because the model needs the stuff stored in temp directory - D)
}

#non looping attempt ------------ 
# if you want to test a loop, just type i=1 in the console (or whatever your iterator is)
# and move through it that way, that way you don't have a bunch of repeated code floating around


# then we create another section that loops through each year, creating big mosaics