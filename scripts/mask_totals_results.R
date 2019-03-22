libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils", "dplyr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#create directories
esp_folder <- "data/esp_mask"
urbag_folder <- "data/urbag_mask"

dir.create("data")
dir.create(esp_folder)
dir.create(urbag_folder)

tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

system(paste0("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl/ ", esp_folder))
system(paste0("aws s3 sync s3://earthlab-amahood/data/landfire_urban_ag_water_mask/ ", urbag_folder))
system("aws s3 sync s3://earthlab-amahood/data/ls5_mucc_extent_matched/ data/ls5_extent_matched/")

scene_extent <- raster("data/ls5_extent_matched/trimmed_extent_p42r31.tif")


esp_mask <- raster("data/esp_mask/clipped_binary.tif")
esp_mask <- projectRaster(esp_mask, scene_extent)

freq(esp_mask)

urbag_mask <- raster("data/urbag_mask/lf_msk_rclss1.tif")  
urbag_mask <- projectRaster(urbag_mask, scene_extent)

freq(urbag_mask)






