# Title: Extracting Differenced Veg Indices to Training Points
# Author(s): Dylan Murphy
# Date Created: 7/22/19
# Date Modified: 7/22/19

#### 1. Set Up ####
#Load Packages
libs <- c("raster", "tidyverse", "foreach", "doParallel", "stringr")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Path objects & Directory Setup
dir.create("data")

#paths and s3 download for differenced veg indices
dir.create("data/differenced")

diff_s3_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/"
diff_local_path <- "data/differenced"

system(paste0("aws s3 sync ", diff_s3_path, " ", diff_local_path))

#paths and s3 download for training_data
dir.create("data/training_points")

training_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/spatially_balanced_points/"
training_local_path <- "data/training_points"

system(paste0("aws s3 sync ", training_s3_path, " ", training_local_path))

#Source Scripts (functions)
source("scripts/functions.R")

#### 2. Data Prep ####

#read in training data 

train <- st_read("data/training_points/gbd_plots_manual_naip_test_humbdolt_Jul2.gpkg")


