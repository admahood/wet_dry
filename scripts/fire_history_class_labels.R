# Title: Time Series Class Labels for Cheatgrass/Sagebrush Based on Fire History
# Author(s): Dylan Murphy
# Date last modified: 

#### Set Up: Load Packages and pull data from s3

#packages
libs <- c("raster", "sf", "dplyr", "doParallel")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#s3 syncs
system("aws s3 cp s3://earthlab-amahood/data/gbd_plots_w_precip_5_20_19_points.gpkg data/blm/gbd_plots.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/fire_perimeters data/fire_perims")

fire_perims <- st_read("data/fire_perims/dissolve_mtbs_perims_1984-2015_DD_20170501.shp")

blm_points <- st_read("data/blm/gbd_plots.gpkg")

crs <- st_crs(blm_points)
fire_perims <- st_transform(fire_perims, crs)

extent <- st_make_grid(blm_points, n = 1)
gb_fire_perims <- st_intersection(fire_perims, extent)


