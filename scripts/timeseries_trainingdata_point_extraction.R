#Title: Time Series Training Data Point Extraction - Landsat & PRISM Climate Data
#Author(s): Dylan Murphy
#Date Last Modified: 6/3/19

# 1. Set Up

#Packages

libs  <- c("raster", "sf", "dplyr", "tidyverse", "doParallel", "foreach", "stringr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts 

source("/home/rstudio/wet_dry/scripts/functions.R")

# S3 syncs

system("aws s3 cp s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_w_lyb_timeseries_Jun5.gpkg data/gbd_plots_w_lyb_timeseries_Jun5.gpkg")
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


gb_plots <- st_read("data/gbd_plots_w_lyb_timeseries_Jun5.gpkg") %>% dplyr::select(-PATH, -ROW, -path_row)

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

#grab vector of all unique objectid values as an iterator
objectid_vec <- unique(result$OBJECTID)

#start counter at 1 at beginning of loop 
counter = 1

#loop over each objectid and year combo (unique to one time series point) and collapse duplicate points into one 
for (i in 1:length(objectid_vec)) { 
  
  #grab one point's time series with a unique objectid (a set of points with differing years and potential duplicates)
  objectid_group <- result[result$OBJECTID == objectid_vec[i],]
  
  #grab years present in selected point's time series as a secondary iterator
  point_years <- unique(objectid_group$plot_year)
  
  #loop over each year and isolate the points for each year within the time series for band value aggregation
    for (j in 1:length(point_years)) {
      
      #check to make sure that the selected point has a time series & or duplicates
      if (length(objectid_group$plot_year) > 1) {
      
      #select duplicated points from the same year which lie in more than one landsat scene 
      objectid_pair <- objectid_group[objectid_group$plot_year == point_years[j],] 
      objectid_pair <- objectid_pair[!is.na(objectid_pair$sr_band1),]
      
      #aggregate band values from different scenes for the pair of duplicate points by taking the average 
      objectid_pair <- objectid_pair %>% dplyr::mutate(sr_band1 = mean(objectid_pair$sr_band1),
                                                       sr_band2 = mean(objectid_pair$sr_band2),
                                                       sr_band3 = mean(objectid_pair$sr_band3),
                                                       sr_band4 = mean(objectid_pair$sr_band4),
                                                       sr_band5 = mean(objectid_pair$sr_band5),
                                                       sr_band7 = mean(objectid_pair$sr_band7))
      #remove duplicated point
      objectid_pair <- objectid_pair[1,] 
      
      #create dataframe and attach collapsed point to it 
      if(counter == 1){
        result2 <- objectid_pair
      }else{
        result2 <- rbind(result2, objectid_pair)
      }  
      
      #advance the counter value
      counter <- counter + 1
  } else {
  
  #if there is only one point for a particular objectid: attach the point to the main training dataframe 
  #(this occurs when it burned too recently to have a time series and lies only within one scene),
  if(!is.na(objectid_group$sr_band1)) {
  if(counter == 1){
    result2 <- objectid_group
  }else{
    result2 <- rbind(result2, objectid_group)
  }
  
  #advance the counter and print message that aggregation is skipped
  print("point lies within one scene only")
  counter <- counter + 1
  } else {
    print("only one point, point lies outside of ls scene")
  }
  }
  }
  }

#check to ensure that the points have been collapsed and no points were lost/duplicated from the original plot data
length(result2$OBJECTID) == length(gb_plots$OBJECTID)

st_write(result2, "data/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg")

system("aws s3 cp data/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg s3://earthlab-amahood/data/training_plots_timeseries/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg")
#### EXTRACTING ANNUAL PRECIP DATA TO TRAINING POINT TIME SERIES ####

system("aws s3 cp s3://earthlab-amahood/data/training_plots_timeseries/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg data/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg")
system("aws s3 sync s3://earthlab-amahood/data/PRISM_precip_annual/greatbasin_trimmed_anomaly_training data/precip_annual/greatbasin_trimmed_anomaly_training")

gbd <- st_read("data/gb_plots_timeseries_w_landsat_noprecip_Jun13.gpkg", quiet=T) %>% mutate(year_factor = as.numeric(as.factor(plot_year)))
  



####create objects for full great basin precip anomaly rasters
training_anomaly_paths <- list.files("data/precip_annual/greatbasin_trimmed_anomaly_training", full.names = T)
training_anomaly <- list()
for(i in 1:length(training_anomaly_paths)) {
  training_anomaly[i] <- raster(training_anomaly_paths[i])
}

#### extract precip anomaly to training data points with matching year 
precip_anomaly_vec <- c()
for(i in 1:nrow(gbd)) {
  precip_year <- as.numeric(gbd[i,]$year_factor)
  precip_anomaly_vec[i] <- raster::extract(training_anomaly[[precip_year]], gbd[i,])
}


#### attach annual precip anomaly to gb training data
gbd <- dplyr::mutate(gbd, precip_anomaly = precip_anomaly_vec) %>% select(-year_factor)

#### save new blm data with precip as a spatially registered gpkg and upload to s3 (use this for future variable extraction)
st_write(gbd, dsn = "data/gbd_plots_timeseries_w_landsat_and_precip_Jun13.gpkg")
system("aws s3 cp data/gbd_plots_timeseries_w_landsat_and_precip_Jun11.gpkg s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_timeseries_w_landsat_and_precip_Jun11.gpkg")

#### save new blm data with precip as a csv and upload to s3
write.csv(gbd, file = "data/gbd_plots_timeseries_w_landsat_and_precip_Jun13.csv")
system("aws s3 cp data/gbd_plots_timeseries_w_landsat_and_precip_Jun11.csv s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_timeseries_w_landsat_and_precip_Jun11.csv")


