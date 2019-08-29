#Title: Creating "Checkerboard" Raster from Landsat pixel grid for identifying mixed pixels using NAIP data
#Author(s): Dylan Murphy
#Date Created: 8/29/19
#Date Modified: 8/29/19

#### 1. Set Up ####

#Install/Load Packages

install.packages("sjmisc")
library(raster)
library(sjmisc)
library(dplyr)

#Create Directories

dir.create("data")
dir.create("data/landsat")
dir.create("data/mask")

#Load Data

#Mean composite Landsat ARD scene (for extent and pixel grid)
system("aws s3 cp s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites/mc_2010summer.tif data/landsat")

#ESP Mask
system("aws s3 cp s3://earthlab-amahood/wet_dry/input_raster_data/landfire_esp_rcl/clipped_binary.tif data/mask")

system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/naip data/naip")

mround <- function(x,base){ 
  base*round(x/base) 
} 

#### 2. Convert ARD scene to binary checkerboard ####
x <- raster("data/landsat/mc_2010summer.tif", band = 1)

x.val <- values(x)

x.new_val <- c()
counter = 1
rowcounter = 0

for(i in 1:length(x.val)){
  if(i %% 5000 == 0) {
  rowcounter <- rowcounter + 1  
  } 
  if(is_odd(rowcounter)) {
  if(is_odd(counter)) {
    x.new_val[i] <- 1 
  } else {
    x.new_val[i] <- 0
  }
  counter = counter + 1
  }else if (is_even(rowcounter)) {
  if(is_odd(counter)) {
    x.new_val[i] <- 0 
  } else {
    x.new_val[i] <- 1
  }
  counter = counter + 1 
}
  }
values(x) <- x.new_val

####3. Mask to sagebrush biophysical zone using ESP Binary Raster & push to s3 ####
mask <- raster("data/mask/clipped_binary.tif") %>% projectRaster(crs = crs(x))

z <- mask(x, mask)

writeRaster(x, filename = "data/ls5_ard_grid.tif", format = "GTiff", overwrite = T)
system("aws s3 cp data/ls5_ard_grid.tif s3://earthlab-amahood/wet_dry/derived_raster_data/ls5_grid/ls5_ard_grid.tif")
