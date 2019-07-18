#Title: "Differenced" NDVI -- Landsat Phenology
#Author(s): Dylan Murphy
#Date Started:  7/17/19
#Date Last Modified: 7/17/19

#### 1. Set Up ####

#Load Packages
libs <- c("raster", "dplyr", "sf")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Create Directories
dir.create("data")
dir.create("data/phenology")

# Pull Data from AWS S3 
system("aws s3 sync s3://earthlab-amahood/data/landsat_phenology data/phenology")

# Source scripts (functions)
source("scripts/functions.R")

#### 2. Data Prep ####
ls_scene_paths <- list.files("data/phenology", full.names = T)
years <- 1984:2011

#create empty list to store landsat sracks
ls_phenology_rasters <- list()

#untar landsat files and create raster stack from tif files 
for(i in 1:length(ls_scene_paths)) {
  year <- substr(ls_scene_paths[i], 26, 29)
  untar(ls_scene_paths[i], exdir = paste0("data/phenology/tifs"))
  }

ls_tifs <- list.files(paste0("data/phenology/tifs", year), pattern = "\\.tif$", full.names = T)
ls_phenology_rasters[i] <- stack(ls_tifs)

