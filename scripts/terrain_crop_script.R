# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#s3 sync -------------------------------- 
s3_path <- "s3://earthlab-amahood/data/ls5_pixel_replaced_crop_test"
local_path <- "home/rstudio/wet_dry/data/terrain_crop_test"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2/terrain_gb.tif"
local_terrain <- "home/rstudio/wet_dry/data/terrain_2/terrain_gb.tif"
system(paste0("aws s3 sync ", s3_path, " ", local_path))

system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#cropping and stacking loop --------------------------
dir.create("data/results")


ls5_list <- list.files(local_path) 
terrain <- brick(local_terrain)

for(i in 1:length(ls5_list)) {
  ls5_noterrain <- raster(paste0(local_path, i))
  terrain_crop <- crop(terrain, extent(ls5_noterrain))
  ls5_terrain <- stack(ls5_noterrain, terrain_crop)
  outname <- sub(pattern     = ".tif",
                 replacement = "_terrain.tif", 
                 x           = ls5_list[i])
  writeRaster(ls5_terrain, paste0("data/results/", outname))
}