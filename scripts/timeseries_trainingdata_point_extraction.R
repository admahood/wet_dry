#Title: Time Series Training Data Point Extraction - Landsat & PRISM Climate Data
#Author(s): Dylan Murphy
#Date Last Modified: 6/3/19

# 1. Set Up

#Packages

libs <- c("raster", "sf", "dplyr", "tidyverse", "doParallel", "foreach")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts 

source("/home/rstudio/wet_dry/scripts/functions.R")

# S3 syncs

system("aws s3 cp s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_w_lyb_timeseries_Jun3.gpkg data/gbd_plots_w_lyb_timeseries_Jun3.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")

# Paths 

landsat_s3 <- "s3://earthlab-amahood/data/landsat_5and7_pixel_replaced_Jun3/"
landsat_local <- "/home/rstudio/wet_dry/data/landsat_5and7_pixel_replaced_Jun3/" 
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

# 2. Data Prep

gb_plots <- st_read("data/gbd_plots_w_lyb_timeseries_Jun3.gpkg")
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp")

years <- unique(gb_plots$plot_year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row)

gbplots_list <- list()


#### Unmodified loop from wet_dry_initial_script_sortingbydate_Feb19.R for landsat band extraction to points 

for(i in 1:length(years)){ 
  if(years[i] >= 2011) { ls_platform <- 7 } else {ls_platform <- 5}
  
  system(paste0("aws s3 cp", " ", landsat_s3, "ls", ls_platform, "_", years[[i]])) #WORK ENDED HERE 6/3
  
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

