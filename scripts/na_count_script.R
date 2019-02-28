#Load packages
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils", "dplyr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#create directories
esp_folder <- "data/esp_nacount"
urbag_folder <- "data/urbag_nacount"

dir.create("data")
dir.create(esp_folder)
dir.create(urbag_folder)

tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

#s3 sync
s3_path <- "s3://earthlab-amahood/data/ls5_mucc"
local_path <- "data/ls5_mucc"

system(paste0("aws s3 sync ", s3_path, " ", local_path))

#load masks and landsat 
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)


#create individual annual esp masks and write to disk
for(i in 1:length(scene_full))  {
  ls5 <- stack(scene_full[i])
  esp_mask <- raster("data/esp_binary/clipped_binary.tif")
  esp_mask <- projectRaster(esp_mask, ls5, res = 30)
  system(paste0("echo", "reprojected", scene_full[i]))
  writeRaster(esp_mask, filename = paste0(esp_folder, "/esp_", substr(scene_full[i], 19, 29), ".tif"))
  system(paste0("echo", "saved", scene_full[i]))
  gc()
}

system(paste0("aws s3 sync", " ", esp_folder, "/", " s3://earthlab-amahood/data/annual_esp_masks_mucc/"))

#create individual annual urb_ag masks and write to disk
for(i in 1:length(scene_full))  {
  ls5 <- stack(scene_full[[i]])
  urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
  urb_mask <- projectRaster(urb_mask, ls5, res = 30)
  system(paste("echo", "reprojected", scene_full[i]))
  
  matrix <- c(1, 0, 0, 1) #switch so that 0 is mask value
  rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
  urb_mask <- reclassify(urb_mask, rclss_matrix)
  system(paste("echo", "reclassified", scene_full[i]))
  
  writeRaster(urb_mask, filename = paste0(urbag_folder, "/urb_", substr(scene_full[i], 19, 29), ".tif"))
  system(paste("echo", "saved", scene_full[i]))
  
  gc()
}

system(paste0("aws s3 sync", " ", urbag_folder, "/", " s3://earthlab-amahood/data/annual_urb_masks_mucc/"))


#s3 sync if above steps have already been done 
system(paste0("aws s3 sync", " ", "s3://earthlab-amahood/data/annual_esp_masks_mucc/ ", esp_folder, "/"))
system(paste0("aws s3 sync", " ", "s3://earthlab-amahood/data/annual_urb_masks_mucc/ ", urbag_folder, "/"))

#get rid of esp values between 0 and 1 from reprojection step. since mask value = 0, everything above 0 does not get masked, therefore to keep it binary i change all of these values to 1(nonmasked value)


esp_list <- list.files(paste0(esp_folder), full.names = T)

esp_rclssed <- list()
for(i in 1:length(esp_list)) {
  esp_rclssed[[i]] <- raster(esp_list[i])
  esp_rclssed[[i]][esp_rclssed[[i]] > 0] <- 1
}

#find extent which covers all masks so we can stack them
xmin <- as.vector(c())
xmax <- as.vector(c())
ymin <- as.vector(c())
ymax <- as.vector(c())
e <- c()

for (i in 1:length(esp_rclssed)) {
  xmin[i] <- extent(esp_rclssed[[i]])@xmin
  xmax[i] <- extent(esp_rclssed[[i]])@xmax
  ymin[i] <- extent(esp_rclssed[[i]])@ymin
  ymax[i] <- extent(esp_rclssed[[i]])@ymax
}

xm <- min(xmin)
xma <- max(xmax)
ym <- min(ymin)
yma <- max(ymax)

e <- matrix(c(xm, ym, xma, yma), nrow = 2, ncol = 2)
e <- extent(e)

for(i in 1:length(esp_rclssed)) { 
  esp_rclssed[[i]] <- extend(esp_rclssed[[i]], e, value = 0)
  gc()
  }

#create stack of esp masks for addition (any pixel that does not add to 28 was esp masked at some point in the model application)
#because mask value = 0, pixels that were never masked should have a value of 1 for all 28 years, and thus upon adding up the stack we can tell which were NA

esp_nacount_stack <- stack(esp_rclssed)
rm(esp_rclssed)
esp_nacount_raster <- sum(esp_nacount_stack)
writeRaster(esp_nacount_raster, filename = paste0(esp_folder, "/detailed_allyears_esp_nacount.tif"))
system(paste0("aws s3 sync", " ", esp_folder, " ", " s3://earthlab-amahood/data/annual_esp_masks_mucc/"))



m <- c(0, 27, 0,  27.5, 28, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

esp_nacount_binary <- reclassify(esp_nacount_raster, rclmat)
writeRaster(esp_nacount_binary, filename = paste0(esp_folder, "/binary_allyears_esp_nacount.tif"))
system(paste0("aws s3 sync", " ", esp_folder, " ", " s3://earthlab-amahood/data/annual_esp_masks_mucc/"))



# URB AG NA COUNT SCRIPT STARTS HERE -----

urb_list <- list.files(paste0(urbag_folder), full.names = T)

urb_rclssed <- list()
for(i in 1:length(urb_list)) {
  urb_rclssed[[i]] <- raster(urb_list[i])
  urb_rclssed[[i]][urb_rclssed[[i]] > 0] <- 1
}

#find extent which covers all masks so we can stack them
xmin <- as.vector(c())
xmax <- as.vector(c())
ymin <- as.vector(c())
ymax <- as.vector(c())
e <- c()

for (i in 1:length(urb_rclssed)) {
  xmin[i] <- extent(urb_rclssed[[i]])@xmin
  xmax[i] <- extent(urb_rclssed[[i]])@xmax
  ymin[i] <- extent(urb_rclssed[[i]])@ymin
  ymax[i] <- extent(urb_rclssed[[i]])@ymax
}

xm <- min(xmin)
xma <- max(xmax)
ym <- min(ymin)
yma <- max(ymax)

e <- matrix(c(xm, ym, xma, yma), nrow = 2, ncol = 2)
e <- extent(e)

for(i in 1:length(urb_rclssed)) { 
  urb_rclssed[[i]] <- extend(urb_rclssed[[i]], e, value = 0)
  gc()
}

#create stack of urb masks for addition (any pixel that does not add to 28 was urb masked at some point in the model application)
#because mask value = 0, pixels that were never masked should have a value of 1 for all 28 years, and thus upon adding up the stack we can tell which were NA

urb_nacount_stack <- stack(urb_rclssed)
rm(urb_rclssed)
urb_nacount_raster <- sum(urb_nacount_stack)
rm(urb_nacount_stack)
writeRaster(urb_nacount_raster, filename = paste0(urbag_folder, "/detailed_allyears_urb_nacount.tif"))
system(paste0("aws s3 sync", " ", urbag_folder, " ", " s3://earthlab-amahood/data/annual_urb_masks_mucc/"))



m <- c(0, 27, 0,  27.5, 28, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

urb_nacount_binary <- reclassify(urb_nacount_raster, rclmat)
writeRaster(urb_nacount_binary, filename = paste0(urbag_folder, "/binary_allyears_urb_nacount.tif"))
system(paste0("aws s3 sync", " ", urbag_folder, " ", " s3://earthlab-amahood/data/annual_urb_masks_mucc/"))

