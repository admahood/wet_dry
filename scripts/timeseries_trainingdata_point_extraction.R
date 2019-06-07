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
  print(paste0("platform selected", years[i]))
  
  for(j in 1:length(path_row_combos)){
  gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                             & gb_plots$plot_year == years[i],]
  print(paste0("plots subsetted for ", years[i], path_row_combos[j]))
  print(paste(round(counter/125*100), "%")) #progress indicator
  
  ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
  ls_files_annual <- str_subset(ls_files_annual, pattern = fixed(path_row_combos[j]))
  system(paste0("aws s3 cp ", landsat_s3, ls_files_annual[1], " ", landsat_local, ls_files_annual[1]))
  print(paste0("landsat downloaded", years[i], path_row_combos[j]))    
        # now we loop through each tif file and extract the values
       
  ls_stack <- raster::stack(paste0(landsat_local, "ls", ls_platform, "_", substr(ls_files_annual[1], 5, 20))) # this just loads the raster
  names(ls_stack) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")
        
  if(length(gbplots_subset$plot_year) > 0) { 
    gbplots_subset <- raster::extract(ls_stack, gbplots_subset, sp=TRUE)
  print(paste0("band values extracted to points", years[i], path_row_combos[j])) 
  gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
  print(paste0("points transformed to lat long", years[i], path_row_combos[j]))    
  
  file.remove(paste0(landsat_local, ls_files_annual)) # now we delete the tifs
  print(paste0("landsat files deleted", years[i], path_row_combos[j]))  
  
  # gbplots_list[[kounter]] <- gbplots_subset
  
  if(kounter == 1){
    result <- gbplots_subset
  }else{
    result <- rbind(result, gbplots_subset)
  }
  print(paste0("points attached to big training dataframe", years[i], path_row_combos[j])) 
  
  kounter <- kounter + 1
  } else {
    print("no points")
  }
    }
    counter <- counter + 1 
  }
  
head(result)


####MISSING LANDSAT FILES? START HERE####
#find any missing landsat files
for (i in 1:length(years)) {
  for (j in 1:length(path_row_combos)) {
    ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
    ls_files_annual <- str_subset(ls_files_annual, pattern = fixed(path_row_combos[j]))
    if(length(ls_files_annual) < 1) { 
     print(paste0(years[i], path_row_combos[j], "missing")) 
       }
  }
}


missing_ls_files <- system("aws s3 ls s3://earthlab-amahood/data/missing_landsat_2008_Jun7/", intern = T)
missing_ls_files <- missing_ls_files[-1]

for (i in 1:length(missing_ls_files)) {
  missing_ls_files[i] <- substr(missing_ls_files[i], 32, 100)
  system(paste0("aws s3 cp s3://earthlab-amahood/data/missing_landsat_2008_Jun7/", missing_ls_files[i], " data/missing_ls_files/", missing_ls_files[i]))
  yr <- substr(missing_ls_files[i], 11, 14)
  prc <- substr(missing_ls_files[i], 5, 10)
  exdir <- "data/ls_tifs"
  dir.create(exdir)
  untar(paste0("data/missing_ls_files/", missing_ls_files[i]), exdir = exdir)
  tifs <- Sys.glob(paste0("data/ls_tifs/", "/*band*.tif"))
  tif_stack <- stack(tifs)
  names(tif_stack) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")
  filename <- paste0("data/missing_ls_files/ls5_", yr, "_", prc, "_.tif")
  writeRaster(tif_stack, filename)
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/data/missing_landsat_2008_Jun7/", substr(filename, 23, 42)))
  a <- list.files("data/missing_ls_files", full.names = T)
  file.remove(list.files("data/missing_ls_files", full.names = T))
  file.remove(list.files("data/ls_tifs", full.names = T))
}
