#Title: Manual Training Data Raster Extraction to Points
#Author(s): Dylan Murphy
#Started: 6/28/19
#Last Modified: 6/28/19

#1: Load Packages & Set Up
libs <- c("raster", "sf", "dplyr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts
source("/home/rstudio/wet_dry/scripts/functions.R")

#s3 syncs
dir.create("data/training_points/")

system("aws s3 sync s3://earthlab-amahood/data/naip_trainingdata/ /home/rstudio/wet_dry/data/training_points")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")

gb_plots <- st_read("data/training_points/progress_check_Jun28.shp")


