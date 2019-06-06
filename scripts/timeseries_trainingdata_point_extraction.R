#Title: Time Series Training Data Point Extraction - Landsat & PRISM Climate Data
#Author(s): Dylan Murphy
#Date Last Modified: 6/3/19

# 1. Set Up

#Packages

libs <- c("raster", "sf", "dplyr", "tidyverse", "doParallel", "foreach", "stringr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts 

source("/home/rstudio/wet_dry/scripts/functions.R")

# S3 syncs

system("aws s3 cp s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_w_lyb_timeseries_Jun5.gpkg data/gbd_plots_w_lyb_timeseries_Jun3.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")

# Paths 

landsat_s3 <- "s3://earthlab-amahood/data/landsat_5and7_pixel_replaced_Jun3/"
landsat_local <- "data/landsat_5and7_pixel_replaced_Jun3/" 
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

# 2. Data Prep
plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp") #raw BLM plots for grabbing crs


gb_plots <- st_read("data/gbd_plots_w_lyb_timeseries_Jun3.gpkg") %>% dplyr::select(-PATH, -ROW, -path_row)

#create scenes object, reproject, get rid of rows we dont want 
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp")
scenes <- st_transform(scenes,st_crs(gb_plots))
scenes <- scenes[scenes$ROW<100,]

#find path/row combo(s) for each point
gb_plots <- st_intersection(gb_plots,scenes[,9:10])
gb_plots$path_row <- as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 

years <- unique(gb_plots$plot_year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row)

ls_files_list <- system("aws s3 ls s3://earthlab-amahood/data/landsat_5and7_pixel_replaced_Jun3/", intern = T)

for(l in 1:length(ls_files_list)) {
  ls_files_list[l] <- substr(ls_files_list[l], 32, 51)
}



gbplots_list <- list()



#### modified loop from wet_dry_initial_script_sortingbydate_Feb19.R for landsat band extraction to points 
###Issue with year. one year iteration works for all PRCs but then the loop fails. Dylan - 6/5/19
counter = 1
kounter = 1
for(i in 1:length(years)){ 
  if(years[i] >= 2011) { ls_platform <- 7 } else {ls_platform <- 5}
  
  for(j in 1:length(path_row_combos)){
  gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                             & gb_plots$plot_year == years[i],]
  print(paste(round(counter/125*100), "%")) #progress indicator
  
  ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
  ls_files_annual <- str_subset(ls_files_annual, pattern = fixed(path_row_combos[j]))
  system(paste0("aws s3 cp ", landsat_s3, ls_files_annual, " ", landsat_local, ls_files_annual[k]))
      
        # now we loop through each tif file and extract the values
       
        ls_stack <- raster::stack(paste0(landsat_local, ls_files_annual)) # this just loads the raster
        names(ls_stack) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")
        
        gbplots_subset <- raster::extract(ls_stack, gbplots_subset, sp=TRUE)
     
      gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
      
      
      file.remove(paste0(landsat_local, ls_files_annual)) # now we delete the tifs
      print("extracted")
    
      gbplots_list[[kounter]] <- gbplots_subset
      
      if(kounter == 1){
        result <- gbplots_subset
      }else{
        result <- rbind(result, gbplots_subset)
      }
      
      
      kounter <- kounter + 1
    }
    counter <- counter + 1 
    
  }
  head(resultd)
