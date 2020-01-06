################################### script for the wet_dry project ###################################
# Authors: Adam Mahood and Dylan Murphy
# Began: Sept 2017
# Last Modified: feb19
#


# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

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
      dates<- substr(tar_files, 56,59) %>%
        as.numeric()
      
      print("untarring")
      untar(tar_files[which.max(dates)], exdir = exdir)
      #listing only the tif files we've extracted (there's a bunch of metadata etc too, which you should familiarize yourself with)
      tif_files <- Sys.glob(paste0(exdir,"*.tif"))
      tif_names <- substr(tif_files, 53, nchar(tif_files)-4)
      
      for(k in 1:length(tif_files)){
        # now we loop through each tif file and extract the values
        band <- raster::raster(tif_files[k]) # this just loads the raster
        names(band) <- tif_names[k]
        gbplots_subset <- raster::extract(band, gbplots_subset, sp=TRUE)
        
        if(startsWith(tif_names[k],"sr_band")){
          gbplots_subset$newcol <- mean(getValues(band), na.rm=T)
          
          names(gbplots_subset)[length(names(gbplots_subset))] <- paste0("mean_", tif_names[k])
        }
      }
      gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
      file.remove(Sys.glob(paste0(exdir,"*"))) # now we delete the tifs
      print("extracted")
      
      gbplots_subset <- gbplots_subset %>%
        dplyr::select(-ends_with("th_band4"))
      
      
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


####NEW LANDFIRE DEM HERE ----------------------------------------------------------
system("aws s3 sync s3://earthlab-amahood/data/LF_DEM /home/rstudio/wet_dry/data/terrain_2")

# terrain raster --------------------------------------------------------------------------
ter_s3 <- 's3://earthlab-amahood/data/terrain_2'
ter_local <- '/home/rstudio/wet_dry/data/terrain_2'

system(paste("aws s3 sync",
             ter_s3,
             ter_local))
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
# keep only sunny days ----------------------------------------------------------------------

df <- result[result$pixel_qa == 66, ] %>% #select plots without clouds
  na.omit()

# Create vegetation indices -------------------------------------------------------------------
df$ndvi <- get_ndvi(df$sr_band3,df$sr_band4)
df$evi <- get_evi(df$sr_band1, df$sr_band3, df$sr_band4)
df$savi <- get_savi(df$sr_band3,df$sr_band4)
df$sr <- get_sr(df$sr_band3,df$sr_band4)

df$greenness <- green7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$brightness <- bright7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$wetness <- wet7(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)

# extracting elevation and topography -----------------------------------------------

df$elevation <- raster::extract(raster("data/terrain_2/lf_dem_reproj_full.tif"), df)
df$slope <- raster::extract(raster("data/terrain_2/slope.tif"), df)
df$aspect <- raster::extract(raster("data/terrain_2/aspect.tif"), df)
df$tpi <- raster::extract(raster("data/terrain_2/TPI.tif"), df)
df$tri <- raster::extract(raster("data/terrain_2/TRI.tif"), df)
df$roughness <- raster::extract(raster("data/terrain_2/roughness.tif"), df)
df$flowdir <- raster::extract(raster("data/terrain_2/flowdir.tif"), df)

# doing the mask thing

system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
shrub_binary <- raster("data/landfire_esp_rcl/shrub_binary.tif") 

df$esp_mask <- raster::extract(binary_clip, df)

# writing the file and pushing to s3 -----------------------------------------------
st_write(df, "data/plots_with_landsat.gpkg", delete_dsn = T)
system("aws s3 cp data/plots_with_landsat.gpkg s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg")

