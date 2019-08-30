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

#### 2. Convert ARD scene to binary checkerboard ####

#create raster object for converion into checkerboard
x <- raster("data/landsat/mc_2010summer.tif", band = 1)

#store values of raster in a new object to iterate over each pixel
x.val <- values(x)

#create empty vector object to store new values
x.new_val <- c()

#set main counter to 1
counter = 1

#set row counter to 0
rowcounter = 0

#loop over each pixel in turn and convert to alternating 0 and 1 values
for(i in 1:length(x.val)){
  if(i %% 5000 == 0) {           #use modulus division to determine when the values in the vector begin a new row
  rowcounter <- rowcounter + 1   #add 1 to row counter when a new row is encountered
  } 
  if(is_odd(rowcounter)) {       #if the rowcounter is odd, start with 1; if even, start with 0 (to create checkerboard vs vertical stripes)
  if(is_odd(counter)) {
    x.new_val[i] <- 1           #if main counter is odd, give the pixel a value of 1
  } else {
    x.new_val[i] <- 0            # if even, give value of 0
  }
  counter = counter + 1         #advance main counter
  }else if (is_even(rowcounter)) {   #if row counter is even, start with 0
  if(is_odd(counter)) {
    x.new_val[i] <- 0           #if main counter is odd (and row counter is even), give pixel a value of zero
  } else {
    x.new_val[i] <- 1           #if main counter and row counter are both even, give pixel a value of 1
  }
  counter = counter + 1         #advance main counter  
}
  }

#set values of raster x to new binary checker pattern values
values(x) <- x.new_val

####3. Mask to sagebrush biophysical zone using ESP Binary Raster & push to s3 ####

#create esp mask object & match projection of checkerboard raster
mask <- raster("data/mask/clipped_binary.tif") %>% projectRaster(crs = crs(x))

# resample mask to match resolution and extent of checkerboard
mask <- resample(mask, x)

#mask checkerboard raster to include only pixels in the big sagebrush esp zone
z <- mask(x, mask, maskvalue = 0)

#write checkerboard raster to disk and push to s3 bucket
writeRaster(z, filename = "data/ls5_ard_grid.tif", format = "GTiff", overwrite = T)
system("aws s3 cp data/ls5_ard_grid.tif s3://earthlab-amahood/wet_dry/derived_raster_data/ls5_grid/ls5_ard_grid.tif")

#### END ####