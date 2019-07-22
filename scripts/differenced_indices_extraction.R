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
dir.create("data/differenced")

diff_s3_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices/"
diff_local_path <- "data/differenced"

system(paste0("aws s3 sync ", diff_s3_path, " ", diff_local_path))

