################################### script for the wet_dry project ###################################
# Authors: Adam Mahood and Dylan Murphy
# Began: Sept 2017
# Last Modified:
#
# Question: does precipitation affect the probability of fire differently in sagebrush shrublands 
# and cheatgrass-dominated annual grasslands? -- how does veg structure affect disturbance probability 
# -- i.e. resilience
#
# Hypothesis: Annual grasslands will maintain a relatively high burn probability across precipitation
# levels, whereas shrublands will have significantly lower probabilities in dry years than wet years.
#
# Basic idea for the workflow: -- steps 1 & 2 possibly feasable by end of semester
# 1. use BLM plot data to classify landsat pixels as either shrubland or grassland
#     1a. clip to great basin
#     a. download landsat images for each year that blm plots were monitored somewhere around peak green
# (joe analytics hub guy is downloading landsat)
#     b. extract landsat band values for each plot location and add to the data frame
#     c. use machine learning/random forests/soemthing better? (account for autocorrelation?) to classify
#        landsat pixels
# 2. then classify all (or some number of random points) landsat pixels for the study area for each year (1984-2014)
#     a. mask ag roads water etc    clouds
#     i. identify points in time where pixels switched from shrub to grassland?
# 3. use fire data - mtbs, baecv - to figure out burn probabilities (need to figure out exactly what this means)
#         i. some kind of thing where there is a timeline of sorts for every plot?
#         ii. number of fires before and after transition points?
# 4. extract climate data to each point for each year
#     a. antecedent precipitation 1 and 2 winters prior
#
# 5. time series -- convergent cross mapping
#
#
# Data structure: long form. each line will have a point, year, veg state
#
#
# visualization idea: animation illustrating the difference

# BEFORE DOING ANYTHING, CLICK THE R PROJECT FILE TO OPEN THE PROJECT
# BEFORE DOING ANYTHING, CLICK THE R PROJECT FILE TO OPEN THE PROJECT
# BEFORE DOING ANYTHING, CLICK THE R PROJECT FILE TO OPEN THE PROJECT
# BEFORE DOING ANYTHING, CLICK THE R PROJECT FILE TO OPEN THE PROJECT

# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
# install.packages("aws.s3")
# library(aws.s3)

source("/home/rstudio/wet_dry/scripts/functions.R")

# import data ------------------------------------------------------------------
# syntax for s3 is: aws s3 sync <s3 bucket location> <local location>

system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")

# if these st_reads don't work, you probably didn't open the project yet

plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp") #BLM plots
ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") #ecoregions
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp") #landsat scenes
binary_clip <- raster("data/landfire_esp_rcl/clipped_binary.tif") #clipped (in arcmap) shrub binary

# tweak data -------------------------------------------------------------------
great_basin <- subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain"))


great_basin <- st_transform(great_basin, st_crs(plot_data)) # matching projections
great_basin <- st_union(great_basin) # dissolve into one polygon
gb_plots <- st_intersection(plot_data,great_basin)# clipping to great basin
gb_plots <- gb_plots[!is.na(gb_plots$DateVisite),] # removing rows where the plot was not read
gb_plots$year <- substr(as.character(gb_plots$DateVisite),1,4) # making a year field


# get scenes -------------------------------------------------------------------
scenes <- st_transform(scenes,st_crs(plot_data)) # matching projections
scenes <- scenes[scenes$ROW<100,] # getting rid of path/rows we don't want 
gb_plots <- st_intersection(gb_plots,scenes[,9:10]) # grabbing only the row and path
gb_plots$path_row <- as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 
# creating a handy dandy field that outputs the exact string that is in the filename

# extract landsat data to plots ------------------------------------------------

landsat_s3 <- "s3://earthlab-amahood/data/landsat7"
landsat_local <- "/home/rstudio/wet_dry/data/landsat7" 
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

years <- unique(gb_plots$year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row) # this gives us the unique path row combinations

# this might have to be modified to fit the st_tranform syntax
gb_plots <- st_transform(gb_plots, crs=st_crs(plot_data))

gbplots_list <- list()
counter <- 1 # can't parallelize with this counter system
kounter <- 1
for(i in 1:length(years)){ 
  
  system(paste0("aws s3 sync ",
                landsat_s3, "/landsat_",years[i], " ",
                landsat_local, "/", years[i]))
  
  for(j in 1:length(path_row_combos)){
    
    # subsetting the plots for the year and row/column combination
    print("subsetting")
    gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                               & gb_plots$year == years[i],]
    print(paste(round(counter/125*100), "%")) #progress indicator
    
    if(nrow(gbplots_subset) > 0) {
      # listing all the files with the right year and path/row combo
      
      tar_files <- Sys.glob(paste0(landsat_local,"/", years[i], "/","LE07", 
                                   path_row_combos[j],
                                   "*.tar.gz")) 
      print("untarring")
      untar(tar_files[1], exdir = exdir)
      #listing only the tif files we've extracted (there's a bunch of metadata etc too, which you should familiarize yourself with)
      tif_files <- Sys.glob(paste0(exdir,"*.tif"))
      
      for(k in 1:length(tif_files)){
        # now we loop through each tif file and extract the values
        band <- raster::raster(tif_files[k]) # this just loads the raster
        gbplots_subset <- raster::extract(band, gbplots_subset, sp=TRUE) # and here we extract
      }
      gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
      file.remove(Sys.glob(paste0(exdir,"*"))) # now we delete the tifs
      print("extracted")
      
      #renaming columns
      oldnames <- names(gbplots_subset[56:69])[1:14]
      newnames <- substr(names(gbplots_subset[56:69]), 42,1000)[1:14]
      gbplots_subset <- rename_at(gbplots_subset, vars(oldnames),~ newnames)
      
      #adding the data to a list - converting back to a shapefile will be easy later, 
      #since they have lat and long as data frame columns
      gbplots_list[[kounter]] <- gbplots_subset
      
      if(kounter == 1){
        result <- gbplots_subset
      }else{
        result <- rbind(result, gbplots_subset)
      }
      
      
      kounter <- kounter + 1
    }else {print("nopoints")}
    counter <- counter + 1 
    
    
  }
  system(paste0("rm -r ",landsat_local, "/", years[i] )) #deleting the unneeded landsat files
}


#### merging elevation tifs ----------------------------------------------
dem_s3 <- 's3://earthlab-amahood/data/SRTM_dem'
dem_local <- '/home/rstudio/wet_dry/data/dem'

system(paste("aws s3 sync",
              dem_s3,
              dem_local))
if(!file.exists("data/dem/gb_dem.tif")){
  
  demlist=list.files(dem_local, pattern="tif$", full.names=TRUE)
  
  # for(i in demlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
  
  raster_list <- list()
  for(i in 1:length(demlist)){
    raster_list[[i]] <- raster(demlist[i])
  }
  
  gb_srtm_dem <- do.call(raster::merge, raster_list)
  writeRaster(gb_srtm_dem, "data/gb_dem.tif")
  system("aws s3 cp data/gb_dem.tif s3://earthlab-amahood/data/SRTM_dem/gb_dem.tif")
}

gb_srtm_dem <- raster("data/dem/gb_dem.tif")

# terrain raster --------------------------------------------------------------------------

opts <- c('slope', 'aspect', 'TPI', 'TRI', 'roughness','flowdir')
for(i in 1:length(opts)){
  filename <- paste0("data/",opts[i],".tif")
  if(!file.exists(filename)){
  ter_rst <- terrain(gb_srtm_dem,
                     opt = opts[i],
                     unit = 'degrees',
                     neighbors = 8, 
                     format = 'GTiff',
                     filename = filename)}
  system(paste0("aws s3 cp ",
                filename,
                " s3://earthlab-amahood/",filename))
}

# keep only sunny days ----------------------------------------------------------------------

df <- result[result$pixel_qa == 66, ] %>% #select plots without clouds
  na.omit()

# Create vegetation indices -------------------------------------------------------------------
df$NDVI <- get_ndvi(df$sr_band3,df$sr_band4)
df$EVI <- get_evi(df$sr_band1, df$sr_band3, df$sr_band4)
df$SAVI <- get_savi(df$sr_band3,df$sr_band4)
df$SR = get_sr(df$sr_band3,df$sr_band4)

df$greenness <- green7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$brightness <- bright7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$wetness <- wet7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)

# extracting elevation and topography -----------------------------------------------

df$elevation <- raster::extract(gb_srtm_dem, df)

df$slope <- raster::extract(raster("data/slope.tif"), df)
df$aspect <- raster::extract(raster("data/aspect.tif"), df)
df$TPI <- raster::extract(raster("data/TPI.tif"), df)
df$TRI <- raster::extract(raster("data/TRI.tif"), df)
df$roughness <- raster::extract(raster("data/roughness.tif"), df)
df$flowdir <- raster::extract(raster("data/flowdir.tif"),df)

df$folded_aspect = abs(180 - abs(df$aspect - 225))

# doing the mask thing 

system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
shrub_binary <- raster("data/landfire_esp_rcl/shrub_binary.tif") 

df$esp_mask <- raster::extract(shrub_binary, df)

# writing the file and pushing to s3 -----------------------------------------------
st_write(df, "data/plots_with_landsat.gpkg")
system("aws s3 cp data/plots_with_landsat.gpkg s3://earthlab-amahood/data/plots_with_landsat.gpkg")

