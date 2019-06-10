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

#grab list of landsat files in pixel replaced s3 bucket
ls_files_list <- system("aws s3 ls s3://earthlab-amahood/data/landsat_5and7_pixel_replaced_Jun3/", intern = T)

#subset landsat files down to just the filename (remove date of uploading etc)
for(l in 1:length(ls_files_list)) {
  ls_files_list[l] <- substr(ls_files_list[l], 32, 51)
}

#### EXTRACT LANDSAT BAND VALUES TO POINTS - MODIFIED LOOP FROM WET_DRY_INITIAL_SCRIPT_SORTING_BY_DATE_FEB19 ####
###It's finally working! Dylan - 6/10/19

kounter = 1 #set counter to 1 at beginning of loop 

#Loop over each year and path/row combo and extract band values to points
for(i in 1:length(years)){ 
  
  #Determine whether LS5 or LS7 file is needed for a particular year 
  if(years[i] >= 2011) { ls_platform <- 7 } else {ls_platform <- 5}
  print(paste0("platform selected", years[i]))
  
  #Subset points with a specific path/row and year
  for(j in 1:length(path_row_combos)){
  gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                             & gb_plots$plot_year == years[i],]
  print(paste0("plots subsetted for ", years[i], path_row_combos[j]))
  #print(paste(round(counter/125*100), "%")) #progress indicator
  
  #select proper landsat scene from list of files in s3 bucket
  ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
  ls_files_annual <- str_subset(ls_files_annual, pattern = fixed(path_row_combos[j]))
  ls_files_annual <- str_subset(ls_files_annual, pattern = fixed(paste0("ls", ls_platform)))
  
  #Download proper scene from s3
  system(paste0("aws s3 cp ", landsat_s3, ls_files_annual[1], " ", landsat_local, ls_files_annual[1]))
  print(paste0("landsat downloaded", years[i], path_row_combos[j]))    
  
  
  #create raster stack object from newly downloaded landsat scene & set variable names for each band    
  ls_stack <- raster::stack(paste0(landsat_local, "ls", ls_platform, "_", substr(ls_files_annual[1], 5, 20))) # this just loads the raster
  names(ls_stack) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", "sr_band5", "sr_band7")
  
  #check to make sure there are points for a particular pathrow/year      
  if(length(gbplots_subset$plot_year) > 0) { 
  
  #extract band values to subset of points
  gbplots_subset <- raster::extract(ls_stack, gbplots_subset, sp=TRUE)
  print(paste0("band values extracted to points", years[i], path_row_combos[j])) 
  
  #transform points back to lat-long
  gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
  print(paste0("points transformed to lat long", years[i], path_row_combos[j]))    
  
  #delete landsat scene from ec2 instance to save space
  file.remove(paste0(landsat_local, ls_files_annual)) 
  print(paste0("landsat files deleted", years[i], path_row_combos[j]))  
  
  #combine points subset in dataframe with the rest of the points based on kounter value
  if(kounter == 1){
    result <- gbplots_subset
  }else{
    result <- rbind(result, gbplots_subset)
  }
  print(paste0("points attached to big training dataframe", years[i], path_row_combos[j])) 
  
  #advance the counter value for the next loop iteration
  kounter <- kounter + 1
  } else {
    
  #if there are no points for a particular path/row & year, print "no points"
    print("no points")
  }
    }
}
#landsat bands to time series points extraction done!

#### TAKING MEAN BAND VALUE FOR POINTS WHICH LIE IN MULTIPLE SCENES & COLLAPSING TO ONE POINT ####

objectid_vec <- unique(result$OBJECTID)

duplicated_vec <- duplicated(result$OBJECTID)


for (i in 1:length(objectid_vec)) { 
  objectid_group <- result[result$OBJECTID == objectid_vec[i],]
  }

subtest <- result[result$OBJECTID == objectid_vec[i],]

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
