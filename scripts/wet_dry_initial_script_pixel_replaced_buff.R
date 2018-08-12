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


# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "doParallel", "foreach")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
# install.packages("aws.s3")
# library(aws.s3)

dir.create("data")
tmpd<- paste0("data/tmp")
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

source("/home/rstudio/wet_dry/scripts/functions.R")

# import data ------------------------------------------------------------------
# syntax for s3 is: aws s3 sync <s3 bucket location> <local location>

system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
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

landsat_s3 <- "s3://earthlab-amahood/data/landsat7_pixel_replaced"
landsat_local <- "/home/rstudio/wet_dry/data/landsat7" 
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

years <- unique(gb_plots$year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row) # this gives us the unique path row combinations

gb_plots <- st_transform(gb_plots, crs=st_crs(plot_data))


system(paste0("aws s3 sync ",
              landsat_s3, " ",
              landsat_local,
              " --only-show-errors"))


gbplots_list <- list()
counter <- 1 # can't parallelize with this counter system
kounter <- 1
for(i in 1:length(years)){ 
  for(j in 1:length(path_row_combos)){
   
    # subsetting the plots for the year and row/column combination
    print("subsetting")
    gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                               & gb_plots$year == years[i],]
    print(paste(round(counter/125*100), "%")) #progress indicator
    
    if(nrow(gbplots_subset) > 0) {
      
      tif_file <- paste0(landsat_local,"/ls7_",years[i],"_", path_row_combos[j],"_.tif")
      if(file.exists(tif_file)){
        stk <- raster::stack(tif_file) # this just loads the raster
        gbplots_subset <- st_transform(gbplots_subset, crs(stk, asText = T)) %>%
          st_buffer(dist=100)
        names(stk) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")
        for(k in 1:nlayers(stk)){
          # now we loop through each tif file and extract the values
          gbplots_subset <- raster::extract(stk[[k]], gbplots_subset, sp=TRUE, fun = mean, na.rm=TRUE) # and here we extract
          print(k)
        }
        gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
        unlink(tif_file)
        print("extracted")
        
        gbplots_list[[kounter]] <- gbplots_subset
        
        if(kounter == 1){
          result <- gbplots_subset
        }else{
          result <- rbind(result, gbplots_subset)
        }
        kounter <- kounter + 1
      }else{print(paste("need", years[i], path_row_combos[j]))}
    }else {print("nopoints")}
    counter <- counter + 1 
  }
}
system("rm data/tmp/*")

# keep only sunny days

df <- result[is.na(result$sr_band1) != TRUE, ] %>% #select plots without clouds
  na.omit()


# terrain raster --------------------------------------------------------------------------
ter_s3 <- 's3://earthlab-amahood/data/terrain_2'
ter_local <- '/home/rstudio/wet_dry/data/terrain_2'

system(paste("aws s3 sync",
             ter_s3,
             ter_local))

gb_dem <- raster("data/terrain_2/lf_dem_reproj_full.tif")

opts <- c('slope', 'aspect', 'TPI', 'TRI', 'roughness','flowdir')
cores <- length(opts)
registerDoParallel(cores)

foreach(i = opts) %dopar% {
  filename <- paste0("data/terrain_2/", i,".tif")
  if(!file.exists(filename)){
    ter_rst <- terrain(gb_dem,
                       opt = i,
                       unit = 'degrees',
                       neighbors = 8, 
                       format = 'GTiff',
                       filename = filename)
    system(paste0("aws s3 cp ",
                  filename,
                  " s3://earthlab-amahood/",filename))
  }
}
rm(ter_rst)

# create folded aspect raster
if(!file.exists("data/terrain_2/aspect.tif")){
  aspect <- raster("data/terrain_2/aspect.tif")
  folded_aspect <- get_folded_aspect(aspect)
  writeRaster(folded_aspect, "data/terrain_2/folded_aspect.tif")
  system("aws s3 cp data/terrain_2/folded_aspect.tif s3://earthlab-amahood/data/terrain_2/folded_aspect.tif")
}

# names <- c("lf_dem_reproj_full", "slope", "aspect", "TPI", "TRI", "roughness", "flowdir", "folded_aspect")
# files <- file.path(ter_local, paste0(names, ".tif"))
# 
# registerDoParallel(cores = 8)
# rasters <- lapply(files, raster)
# 
# par_l <- list()
# par_res <-foreach (r = 1:length(rasters)) %dopar% {
#   par_l[[r]] <- raster::extract(rasters[r], df, fun = mean, na.rm=TRUE)
# }

df$elevation <- raster::extract(raster("data/terrain_2/lf_dem_reproj_full.tif"), df,fun = mean, na.rm=TRUE)
df$slope <- raster::extract(raster("data/terrain_2/slope.tif"), df,fun = mean, na.rm=TRUE)
df$aspect <- raster::extract(raster("data/terrain_2/aspect.tif"), df,fun = mean, na.rm=TRUE)
df$tpi <- raster::extract(raster("data/terrain_2/TPI.tif"), df,fun = mean, na.rm=TRUE)
df$tri <- raster::extract(raster("data/terrain_2/TRI.tif"), df,fun = mean, na.rm=TRUE)
df$roughness <- raster::extract(raster("data/terrain_2/roughness.tif"), df,fun = mean, na.rm=TRUE)
df$flowdir <- raster::extract(raster("data/terrain_2/flowdir.tif"), df,fun = mean, na.rm=TRUE)
df$folded_aspect <- raster::extract(raster("data/terrain_2/folded_aspect.tif"), df,fun = mean, na.rm=TRUE)

system("rm data/tmp/*")

# Create vegetation indices -------------------------------------------------------------------
df<- mutate(df,
            ndvi = get_ndvi(sr_band3, sr_band4),
            evi = get_evi(sr_band1, sr_band3, sr_band4),
            savi = get_savi(sr_band3, sr_band4),
            sr = get_sr(sr_band3, sr_band4),
            ndsvi = get_ndsvi(sr_band3, sr_band5),
            greenness = green7(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7),
            brightness = bright7(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7),
            wetness = wet7(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7))

# doing the mask thing
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
df$esp_mask <- raster::extract(raster("data/landfire_esp_rcl/shrub_binary.tif"), df,fun = mean, na.rm=TRUE)

# writing the file and pushing to s3 -----------------------------------------------
st_write(df, "data/plots_with_landsat.gpkg", delete_layer = TRUE)
system("aws s3 cp data/plots_with_landsat.gpkg s3://earthlab-amahood/data/plots_with_landsat_buffed.gpkg")

