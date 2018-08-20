# nts -- clip to extent of the great basin polygon, then make aspect, elevation, etc

# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "doParallel", "foreach")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

dir.create("data")
#grabbing the dem from lanfire
system(paste0("aws s3 cp s3://earthlab-amahood/data/landfire_dem/lf_dem_cus.zip data/lf_dem_cus.zip"))
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")

system("unzip data/lf_dem_cus.zip")

elevation <- raster("DEM_Elevation/Grid/us_dem")


ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") %>%
  st_transform(crs(elevation, asText=TRUE))
great_basin <- ecoregions %>%
  dplyr::filter(NA_L3NAME == "Northern Basin and Range" |
                  NA_L3NAME == "Central Basin and Range"|
                  NA_L3NAME == "Snake River Plain") %>%
  st_union()

scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp") %>%
  st_transform(crs(elevation, asText=TRUE)) %>%
  dplyr::filter(ROW>100)
mat <- st_intersects(scenes, great_basin, sparse=FALSE) #put this on the internet
scenes <- scenes[mat==T,]

elevation_gb <- raster::crop(elevation, scenes)
plot(elevation_gb)

writeRaster(elevation_gb, "data/elevation_gb.tif")
system("aws s3 cp data/elevation_gb.tif s3://earthlab-amahood/data/terrain_gb/elevation.tif")

opt <- c('slope','aspect', 'TPI', 'TRI', 'roughness', 'flowdir')
opt1 <- c('slope','aspect', 'tpi', 'tri', 'roughness', 'flowdir')
corz <- detectCores()
registerDoParallel(corz)

foreach(o = 1:length(opt)) %dopar% {
  ter<- terrain(elevation_gb, 
        opt=opt[o], filename = paste0(opt1[o], ".tif"))
  
  system(paste0("aws s3 cp ",
                opt1[o], ".tif ",
                "s3://earthlab-amahood/data/terrain_gb/", opt1[o], ".tif",
                " --only-show-errors"))
}
