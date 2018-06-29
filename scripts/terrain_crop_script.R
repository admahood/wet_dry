# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#s3 sync -------------------------------- 
s3_path <- "s3://earthlab-amahood/data/ls5_pixel_replaced_crop_test"
local_path <- "data/terrain_crop_test"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_2"
system(paste0("aws s3 sync ", s3_path, " ", local_path))

system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#cropping and stacking loop --------------------------

dir.create("data/results")


ls5_list <- list.files(local_path, full.names = T) 

ls5_files <- list.files(local_path)

# this is unnecessary -- and takes forever
# terrain <- stack(list.files(local_terrain, full.names = T))
# terrain <- brick(terrain)

ter <- lapply(list.files(local_terrain, full.names =T), FUN = raster)
names(ter) <- sub(".tif", "", list.files(local_terrain))

for(i in 1:length(ls5_list)) {
  ls5_noterrain <- stack(ls5_list[i])
  
  filenamet <- paste0("home/rstudio/wet_dry/data/results/",  
                      sub(pattern = ".tif", 
                          replacement = "terrain.tif", 
                          x = ls5_files[i], fixed = T))
  
  terrain_cropped <- crop(terrain, ls5_noterrain)
  
  ls5_terrain <- stack(ls5_noterrain, terrain_cropped)
  
  writeRaster(ls5_terrain, filename = filenamet, overwrite = T)
  
}

#non looping attempt ------------ 
# if you want to test a loop, just type i=1 in the console (or whatever your iterator is)
# and move through it that way, that way you don't have a bunch of repeated code floating around
filename1 <- paste0("data/results/", ls5_list[1], "_terrain.tif")

ls5_noterrain2 <- stack(ls5_list[1])

terrain_cropped2 <- resample(terrain, ls5_noterrain2)

ls5_terrain2 <- stack(ls5_noterrain2, terrain_cropped2)

writeRaster(ls5_terrain, filename = filenamet, progress = 'text')
