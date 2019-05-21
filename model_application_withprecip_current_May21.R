# Title: Random Forest Model Application - GB Land Cover Classification Using Precip Anomaly
# Author(s): Dylan Murphy
# Date last modified: 5/21/19

#### 1.1: Setup - Load Packages/Source Scripts
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")

#### 1.2: Setup - Create Directories
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

#### 1.3: Setup - Pull Data from S3
#landsat paths
s3_ls_path <- "s3://earthlab-amahood/data/ls5_mucc"
local_ls_path <- "data/ls5_mucc"
#terrain paths 
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_reproj_full"
#precip anomaly raster paths (cropped to NAIP scene already)
s3_precip <- "s3://earthlab-amahood/data/PRISM_precip_annual/naip_trimmed_annual_precip_anomaly"
local_precip <- "s3://earthlab-amahood/data/precip_annual/naip_trimmed_annual_precip_anomaly"

#s3 syncs 
system(paste0("aws s3 sync ", s3_ls_path, " ", local_ls_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))
system(paste0("aws s3 sync ", s3_precip, " ", local_precip))

#s3 syncs cont. (masks)
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/data/landfire_urban_ag_water_mask/ data/urban_ag_mask")

#grab filenames for ls5 stacks 
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)

#### 1.4: Setup - Parallelization
cores <- length(scene) / 2 
registerDoParallel(cores)

#### 2. Model Application Loop (Parallized)




