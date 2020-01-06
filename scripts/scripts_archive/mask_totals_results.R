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
# system("aws s3 sync s3://earthlab-amahood/data/ls5_mucc/ data/ls5_mucc/")

scene_extent <- raster("data/extent_matched/trimmed_extent_p42r31.tif")

#how many pixels were masked by esp masking (maskvalue = 0)
esp_mask <- raster("data/esp_mask/clipped_binary.tif")
esp_mask <- projectRaster(esp_mask, scene_extent)

freq(esp_mask)

#how many pixels were masked by urb_ag masking? (maskvalue = 1)
urbag_mask <- raster("data/urbag_mask/lf_msk_rclss1.tif")  
urbag_mask <- projectRaster(urbag_mask, scene_extent)

freq(urbag_mask)

#how many pixels in each original ls5 image (p42r31) were NA due to cloud masking, extent matching, or sensor drift?
ls5_px_repl <- list.files("data/ls5_extent_matched", full.names = T)
clouds_masked <- list()
for (i in 1:length(ls5_px_repl)) {
  ls5 <- raster(ls5_px_repl[i])
  clouds_masked[i] <- as.vector(sum(values(is.na(ls5))))
}

clouds_masked <- unlist(clouds_masked)
mean(clouds_masked) 
sd(clouds_masked)



