#Title: Manual Training Data Raster Extraction to Points Using Landsat ARD mean composite band values
#Author(s): Dylan Murphy
#Started: 6/28/19
#Last Modified: 1/29/19

#### 1: Load Packages & Set Up ####
libs <- c("raster", "sf", "dplyr", "stringr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts
source("/home/rstudio/wet_dry/scripts/functions.R")

#paths 

  #ARD MEAN COMPOSITE PATHS
landsat_s3 <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites_ndvi/"
landsat_local <- "data/landsat_ard"
dir.create("data")
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

#s3 syncs
dir.create("data/training_points/")

system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/manual_training_points/ /home/rstudio/wet_dry/data/training_points")
system("aws s3 sync s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_climate_vars /home/rstudio/wet_dry/data/training_points")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")

#ONLY SYNC THE BELOW DATA IF ADDING VARIABLES TO POINTS W/VARIABLES ALREADY EXTRACTED
#(USE ABOVE SYNCS IF CREATING ENTIRELY NEW TRAINING POINTS)

system("aws s3 sync s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_climate_vars /home/rstudio/wet_dry/data/training_timeseries")

#grab proper crs for reprojection
crs_grab_points <- st_read("data/manual_points_2class_2010_ard_phenology_all_variables_extracted_w_actual_precip_Nov26.gpkg")
ts_crs <- st_crs(crs_grab_points)

#Create training point objects to extract variables to: 

#### 1.1 -- if creating training data from empty points: set ts_year to 2010 ####
#first year in training time series = 2010;
#(see below to change labels and create other years in training timeseries using lyb to change labels):
ts_year <- 2010
gb_plots <- st_read("data/training_points/spatially_balanced_points_ard_phenology/ard_pheno_sbp_points_final_Jul29.shp") %>% mutate(Year = ts_year)

#### 1.2 -- if extracting a new year in a training time series: start with step 1.2.1 below ####
#1.2.1: Changing manually created Point Labels for different years based on fire history

#change labels based on lyb, change year to target year,
#then create gb_plots object from "new_naip_points" object 

#SET YEAR (VERY IMPORTANT)
ts_year <- 2006

#download manually created NAIP point data w/ lyb attached from s3
system("aws s3 sync s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_lyb_extracted/ /home/rstudio/wet_dry/data/training_points_lyb")

#download 2010 point data to grab crs and reproject
system("aws s3 cp s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/ard_pheno_spatially_balanced_points/manual_points_2class_2010_ard_phenology_all_variables_extracted_w_actual_precip_Nov26.gpkg data/manual_points_2class_2010_ard_phenology_all_variables_extracted_w_actual_precip_Nov26.gpkg")

crs_grab_points <- st_read("data/manual_points_2class_2010_ard_phenology_all_variables_extracted_w_actual_precip_Nov26.gpkg")

ts_crs <- st_crs(crs_grab_points)

#read in original (2010) NAIP informed training data w/ lyb attached

naip_points <- st_read("data/training_points_lyb/manual_ard_points_2010_new_vars_and_lyb_Dec10.gpkg") %>% 
  dplyr::select(-plot_year) %>% 
  dplyr::mutate(Year = ts_year) %>%
  st_transform(crs = ts_crs) 

#create new object to modify for a new year of labelling (change year in "mutate" to year desired for modeling/labelling)
new_naip_points <- naip_points %>% 
  filter(Year != lyb | is.na(lyb)) #removing points which burned in the year of interest and keeping those that did not burn


#loop through each point and determine whether a steady state can be assumed for the year of choice 
# (because sagebrush takes a long time to establish, even if it burned in between the target year and 
#the year of original labelling (2010) I assume it was sagebrush in the target year)

#remove points labelled grass which burned after the target year - cannot assume they were grass prior to burning
for(i in 1:nrow(new_naip_points)) {
  if(isTRUE(new_naip_points[i,]$lyb > ts_year & new_naip_points[i,]$Label == "grass")) {
    new_naip_points <- new_naip_points[-i,]
  }
}

#remove previously extracted variables from original year 
new_naip_points <- new_naip_points %>% dplyr::select(OBJECTID, Label, Year, lyb)

#SAVING POINTS WITH NEW LABELS (WITHOUT VARIABLES EXTRACTED)
# #create filename for relabeled points w/o variables (change year to match year of interest)
# new_year_naip_filename <- paste0("data/manual_ard_005007_climare_vars_points_", ts_year, "_no_vars.gpkg")
# 
# st_write(new_naip_points, new_year_naip_filename)
# 
# system(paste0("aws s3 cp ", new_year_naip_filename, " s3://earthlab-amahood/wet_dry/input_vector_data/manual_training_points/spatially_balanced_points_ard_phenology/spb_points_ard_w_lyb_novars/", substr(new_year_naip_filename, 6, 100)))



gb_plots <- new_naip_points %>% mutate(ID = row_number())
#### 1.3 -- if adding variables to already extracted training data: use the below code chunk to read in already created training data and add variables ####
#(as of 1/22/20, adding ndvi informed ard bands and veg indices/diff variables & removing previously extracted)

#set year of training data to which variables are being added
ts_year <- "2006"

gb_plots <- st_read(paste0("data/training_timeseries/manual_points_2class_", ts_year, "_ard_new_climate_vars_Jan3.gpkg")) %>% 
  st_transform(crs = ts_crs) %>% mutate(ID = row_number(),Year = ts_year) 

#remove previously extracted bands, veg indices, tassel cap, and diff indices (only applies to point extraction on 1/22/20)
gb_plots <- gb_plots %>% dplyr::select(-spring_sr_band1, -spring_sr_band2, -spring_sr_band3, -spring_sr_band4, -spring_sr_band5, -spring_sr_band7,
                                       -summer_sr_band1, -summer_sr_band2, -summer_sr_band3, -summer_sr_band4, -summer_sr_band5, -summer_sr_band7,
                                       -spring_ndvi, -spring_evi, -spring_evi, -spring_savi, -spring_sr, -spring_ndti, -spring_green_ndvi, 
                                       -spring_sla_index, -spring_ndi7, -spring_greenness, -spring_brightness, -spring_wetness, 
                                       -summer_ndvi, -summer_evi, -summer_evi, -summer_savi, -summer_sr, -summer_ndti, -summer_green_ndvi, 
                                       -summer_sla_index, -summer_ndi7, -summer_greenness, -summer_brightness, -summer_wetness, 
                                       -diff_evi, -diff_green_ndvi, -diff_ndi7, -diff_ndsvi, -diff_ndti, -diff_ndvi, -diff_satvi, 
                                       -diff_satvi, -diff_savi, -diff_sla_index, -diff_sr
                                       )
   
#### 2. create objects and extract path/row combos to each point ####

plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp")

esp_mask <- raster("data/landfire_esp_rcl/clipped_binary.tif")

#create scene path/row object and remove unneccessary path/rows
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp")
scenes <- st_transform(scenes, as.character(crs(esp_mask)))
scenes <- scenes[scenes$ROW<100,]

#extract pathrow combos to points
gb_plots <- st_intersection(gb_plots,scenes[,9:10]) %>% arrange(ID)
gb_plots$path_row <- as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 

#remove points that lie in more than one scene (duplicated points from st_intersection)
dupl_vec <- duplicated(gb_plots$ID)
gb_plots <- gb_plots %>% mutate(duplicated = dupl_vec) 
gb_plots <- gb_plots[gb_plots$duplicated == F,]

#grab the path row combos needed

path_row_combos <- unique(gb_plots$path_row)

#### 3. LANDSAT ARD BAND EXTRACTION (SPRING & SUMMER BAND VALUES) ####

#set sensor platform used to make mean composites to grab correct composite rasters
sensor_platform <- "ls5_and_ls7" 

#list available pixel replaced landsat files
ls_files_list <- system(paste0("aws s3 ls s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites_ndvi/", sensor_platform, "/"), intern = T)
ls_files_list <- ls_files_list[-1]

#subset ls filenames down to just season, year, and platform
for(l in 1:length(ls_files_list)) {
  ls_files_list[l] <- substr(ls_files_list[l], 35, 69)
}

kounter = 1 #set counter to 1 at beginning of loop 

years <- unique(gb_plots$Year) #grab years present in training data

#Loop over each year (works even if only one year present in training points) 
#and path/row combo and extract band values to points
for(i in 1:length(years)){ 
  
  #Subset points for a specific year
    gbplots_subset <- gb_plots[gb_plots$Year == years[i],]
    print(paste0("plots subsetted for ", years[i]))
    
    #select proper landsat scene from list of files in s3 bucket
    ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
    
    for(j in 1:length(ls_files_annual)) {
    
    season <- substr(ls_files_annual[j], 5, 10)
    #Download proper scene from s3
    system(paste0("aws s3 cp ", landsat_s3, sensor_platform, "/mc_", ls_files_annual[j], " ", landsat_local, "/", ls_files_annual[j]))
    print(paste0("landsat downloaded ", years[i], season))    
    
    
    #create raster stack object from newly downloaded landsat scene & set variable names for each band specifying season of mean composite    
    ls_stack <- raster::stack(paste0(landsat_local, "/", years[i], season, "_ndvi_ts_", sensor_platform, ".tif")) # this just loads the raster
    names(ls_stack) <- c(paste0(season, "_sr_band1"), paste0(season, "_sr_band2"), paste0(season, "_sr_band3"), paste0(season, "_sr_band4"), paste0(season, "_sr_band5"), paste0(season, "_sr_band7"))

    #check to make sure there are points for a particular pathrow/year      
     
      
      #extract band values to subset of points
      gbplots_subset <- raster::extract(ls_stack, gbplots_subset, sp=TRUE)
      print(paste0("band values extracted to points ", years[i])) 
      
      #transform points back to lat-long
      gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = ts_crs)
      print(paste0("points transformed to lat long ", years[i]))    
      
      #delete landsat scene from ec2 instance to save space
      file.remove(paste0(landsat_local, "/", ls_files_annual[j])) 
      print(paste0("landsat files deleted ", years[i], ls_files_annual[j]))  
    }    
      
      #combine points subset in dataframe with the rest of the points based on kounter value
      if(kounter == 1){
        result <- gbplots_subset
      }else{
        result <- rbind(result, gbplots_subset)
      }
      print(paste0("points attached to big training dataframe", years[i])) 
      
      #advance the counter value for the next loop iteration
      kounter <- kounter + 1
}

#### 4. clean up points - remove points with NA extracted values ####
  
#removing NA values (points along the outer edge of landsat scene)
result <- result[!is.na(result$spring_sr_band1),] 
result <- result[!is.na(result$summer_sr_band1),] %>% arrange(ID)

gb_plots <- result
#### 5. PRECIP ANOMALY EXTRACTION ####

#change year in path to year of interest for extraction 
system(paste0("aws s3 cp s3://earthlab-amahood/wet_dry/derived_raster_data/PRISM_precip_anomaly/greatbasin_trimmed_anomaly_training/precip_anomaly_train", year, ".tif", " data/precip_annual/greatbasin_trimmed_anomaly_training/"))



training_anomaly_paths <- list.files("data/precip_annual/greatbasin_trimmed_anomaly_training", full.names = T)
training_anomaly_paths <- str_subset(training_anomaly_paths, pattern = fixed(as.character(year)))

training_anomaly <- list()

precip_anom_raster <- raster(training_anomaly_paths)

#### extract precip anomaly to training data points
precip_anomaly_vec <- c()

for(i in 1:nrow(gb_plots)) {
  precip_anomaly_vec[i] <- raster::extract(precip_anom_raster, gb_plots[i,])
}

# attach annual precip anomaly to gb training data 
gb_plots <- dplyr::mutate(gb_plots, precip_anomaly = precip_anomaly_vec) 

#### 6. ACTUAL PRECIP EXTRACTION ####

#download monthly precip rasters
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/PRISM_precip/PRISM_monthly_precip data/precip")

#create objects and store the folder names and paths of .bil precip rasters
monthly_precip_folders <- list.files("data/precip", full.names = T)
monthly_precip_paths <- list()

for(i in 1:length(monthly_precip_folders)) {
monthly_precip_paths[[i]] <- list.files(monthly_precip_folders[i], pattern = "\\.bil$", full.names = T)
monthly_precip_paths[[i]] <- monthly_precip_paths[[i]][2:13]
}
monthly_precip_paths <- unlist(monthly_precip_paths)

#create vector of abbreviated month names for variable naming

month_names <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


#create vector of variable names for monthly precip
monthly_precip_names <- c()
for(i in 1:length(month_names)) {
  monthly_precip_names[i] <- paste0(month_names[i], "_precip")
}

#create empty list to store vectors of extracted precip values
gb_plots_pts <- list()

#loop over each month's precip raster and extract values at each training point, storing vector result in list
year <- unique(gb_plots$Year)

for(i in 1:length(monthly_precip_names)) {
  if(month_names[i] == "sep") {
    target_path <- stringr::str_subset(monthly_precip_paths, pattern = fixed(paste0(as.character(year - 1), "0", i)))
    one_month_precip <- raster(target_path)
    gb_plots_pts[[i]] <- raster::extract(one_month_precip, gb_plots)
    print(paste0(target_path, " extracted for ", month_names[i]))
} else if (month_names[i] == "oct" | month_names[i] == "nov" | month_names[i] == "dec") {
    target_path <- stringr::str_subset(monthly_precip_paths, pattern = fixed(paste0(as.character(year - 1), i)))
    one_month_precip <- raster(target_path)
    gb_plots_pts[[i]] <- raster::extract(one_month_precip, gb_plots)
    print(paste0(target_path, " extracted for ", month_names[i]))
} else if (month_names[i] == "jan" | month_names[i] =="feb" | month_names[i] == "mar" | month_names[i] == "apr" | month_names[i] == "may" | month_names[i] == "jun" | month_names[i] == "jul" | month_names[i] == "aug") {
    target_path <- stringr::str_subset(monthly_precip_paths, pattern = fixed(paste0(as.character(year), "0", i)))
    one_month_precip <- raster(target_path)
    gb_plots_pts[[i]] <- raster::extract(one_month_precip, gb_plots)
    print(paste0(target_path, " extracted for ", month_names[i]))
  }
}

#convert list of vectors to data frame and attach respective monthly variable names 
gb_plots_pts <- as.data.frame(gb_plots_pts)
names(gb_plots_pts) <- monthly_precip_names

#attach extracted monthly actual precip values to training data
gb_plots <- dplyr::bind_cols(gb_plots, gb_plots_pts)

gb_plots$winter_precip <- gb_plots$dec_precip + gb_plots$jan_precip + gb_plots$feb_precip 
gb_plots$spring_precip <- gb_plots$mar_precip + gb_plots$apr_precip + gb_plots$may_precip 
gb_plots$summer_precip <- gb_plots$jun_precip + gb_plots$jul_precip + gb_plots$aug_precip 
gb_plots$fall_precip <- gb_plots$sep_precip + gb_plots$oct_precip + gb_plots$nov_precip 

#### 7. CLIMATE VARIABLE Z SCORES EXTRACTION ####
#download climate z score layers from s3
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/climate_zscores data/climate")

#list all climate zscore files
zscore_files <- list.files("data/climate", full.names = T)

#create empty vector to store climate variable names for renaming columns later
var_names <- c()
#loop over all zscore files and grab variable names
for(i in 1:length(zscore_files)) {
var_names[i] <- substr(zscore_files[i], 14, 16)
}

#grab unique variable names present (could have made the names manually but this way 
#we wont have to remember to change anything when we add precip z scores)
var_names <- unique(var_names)

#create empty vector to store final column names
zscore_names <- c()

#add "_z" to variable names (i.e. z scores)
for(i in 1:length(var_names)) {
zscore_names[i] <- paste0(var_names[i], "_z") 
}

#grab years present in training data
years <- unique(gb_plots$Year)

#create empty list to store climate z score vectors
climate_zscore_extracted <- list()

#loop over each year in training data, select relevant climate zscore layers, extract 
#values to points, and store resulting vectors in a list
for(i in 1:length(years)) {
  prior_year <- as.numeric(years[i]) - 1
  target_zscore_paths_sameyear <- str_subset(zscore_files, pattern = fixed(years[i]))
  target_zscore_paths_prioryear <- str_subset(zscore_files, pattern = fixed(as.character(prior_year)))
  for(j in 1:length(target_zscore_paths_sameyear)) {
    zscore_layer_sameyear <- stack(target_zscore_paths_sameyear[j])
    zscore_layer_prior_year <- stack(target_zscore_paths_prioryear[j])
  #change the below subsets to grab different months for "water year"
    zscore_layer_water_year <- stack(zscore_layer_prior_year[[9:12]], zscore_layer_sameyear[[1:3]]) 
    zscore_layer <- mean(zscore_layer_water_year[[1:7]]) #change if water year months != 7
    climate_zscore_extracted[[j]] <- raster::extract(zscore_layer, gb_plots)
  }
}

#convert list of vectors to a data frame
climate_zscores <- as.data.frame(climate_zscore_extracted)

#attach appropriate names to climate z score variables (columns in data frame)
names(climate_zscores) <- zscore_names

#attach new climate z score variables to main training points
gb_plots <- dplyr::bind_cols(gb_plots, climate_zscores)

#### 8. TERRAIN EXTRACTION ####
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/terrain_2 /home/rstudio/wet_dry/data/terrain_2")



gb_plots$elevation <- raster::extract(raster("data/terrain_2/lf_dem_reproj_full.tif"), gb_plots)
gb_plots$slope <- raster::extract(raster("data/terrain_2/slope.tif"), gb_plots)
gb_plots$aspect <- raster::extract(raster("data/terrain_2/aspect.tif"), gb_plots)
gb_plots$tpi <- raster::extract(raster("data/terrain_2/TPI.tif"), gb_plots)
gb_plots$tri <- raster::extract(raster("data/terrain_2/TRI.tif"), gb_plots)
gb_plots$roughness <- raster::extract(raster("data/terrain_2/roughness.tif"), gb_plots)
gb_plots$flowdir <- raster::extract(raster("data/terrain_2/flowdir.tif"), gb_plots)

#### 9. VEG INDICES AND TASSELLED CAP VARIABLE CALCULATION ####

  #SPRING
gb_plots$spring_ndvi <- get_ndvi(gb_plots$spring_sr_band3,gb_plots$spring_sr_band4)
gb_plots$spring_evi <- get_evi(gb_plots$spring_sr_band1, gb_plots$spring_sr_band3, gb_plots$spring_sr_band4)
gb_plots$spring_savi <- get_savi(gb_plots$spring_sr_band3,gb_plots$spring_sr_band4)
gb_plots$spring_sr <- get_sr(gb_plots$spring_sr_band3,gb_plots$spring_sr_band4)
gb_plots$spring_ndti <- get_ndti(band5 = gb_plots$spring_sr_band5, band7 = gb_plots$spring_sr_band7)
gb_plots$spring_green_ndvi <- get_green_ndvi(band4 = gb_plots$spring_sr_band4, band2 = gb_plots$spring_sr_band2)
gb_plots$spring_sla_index <- get_SLA_index(band3 = gb_plots$spring_sr_band3, band4 = gb_plots$spring_sr_band4, band7 = gb_plots$spring_sr_band7)
gb_plots$spring_ndi7 <- get_ndi7(band4 = gb_plots$spring_sr_band4, band7 = gb_plots$spring_sr_band7)

gb_plots$spring_greenness <- green7(gb_plots$spring_sr_band1,gb_plots$spring_sr_band2,gb_plots$spring_sr_band3,gb_plots$spring_sr_band4,gb_plots$spring_sr_band5,gb_plots$spring_sr_band7)
gb_plots$spring_brightness <- bright7(gb_plots$spring_sr_band1,gb_plots$spring_sr_band2,gb_plots$spring_sr_band3,gb_plots$spring_sr_band4,gb_plots$spring_sr_band5,gb_plots$spring_sr_band7)
gb_plots$spring_wetness <- wet7(gb_plots$spring_sr_band1,gb_plots$spring_sr_band2,gb_plots$spring_sr_band3,gb_plots$spring_sr_band4,gb_plots$spring_sr_band5,gb_plots$spring_sr_band7)

  #SUMMER
gb_plots$summer_ndvi <- get_ndvi(gb_plots$summer_sr_band3,gb_plots$summer_sr_band4)
gb_plots$summer_evi <- get_evi(gb_plots$summer_sr_band1, gb_plots$summer_sr_band3, gb_plots$summer_sr_band4)
gb_plots$summer_savi <- get_savi(gb_plots$summer_sr_band3,gb_plots$summer_sr_band4)
gb_plots$summer_sr <- get_sr(gb_plots$summer_sr_band3,gb_plots$summer_sr_band4)
gb_plots$summer_ndti <- get_ndti(band5 = gb_plots$summer_sr_band5, band7 = gb_plots$summer_sr_band7)
gb_plots$summer_green_ndvi <- get_green_ndvi(band4 = gb_plots$summer_sr_band4, band2 = gb_plots$summer_sr_band2)
gb_plots$summer_sla_index <- get_SLA_index(band3 = gb_plots$summer_sr_band3, band4 = gb_plots$summer_sr_band4, band7 = gb_plots$summer_sr_band7)
gb_plots$summer_ndi7 <- get_ndi7(band4 = gb_plots$summer_sr_band4, band7 = gb_plots$summer_sr_band7)

gb_plots$summer_greenness <- green7(gb_plots$summer_sr_band1,gb_plots$summer_sr_band2,gb_plots$summer_sr_band3,gb_plots$summer_sr_band4,gb_plots$summer_sr_band5,gb_plots$summer_sr_band7)
gb_plots$summer_brightness <- bright7(gb_plots$summer_sr_band1,gb_plots$summer_sr_band2,gb_plots$summer_sr_band3,gb_plots$summer_sr_band4,gb_plots$summer_sr_band5,gb_plots$summer_sr_band7)
gb_plots$summer_wetness <- wet7(gb_plots$summer_sr_band1,gb_plots$summer_sr_band2,gb_plots$summer_sr_band3,gb_plots$summer_sr_band4,gb_plots$summer_sr_band5,gb_plots$summer_sr_band7)

#### 10. Extracting Differenced Veg. Indices (Phenology Variables) ####

#create directory to store differenced veg index rasters
dir.create("data/diff_indices")

#download differenced veg indices from amazon s3 bucket
s3_differenced_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices"
system(paste0("aws s3 sync ", s3_differenced_path, " ", "data/diff_indices"))

#list folders with differenced veg indices
diff_folders <- list.files("data/diff_indices", full.names = T)

#create empty vector to store differenced index names for changing names later on
diff_index_names <- c()

#create vector of differenced index variable names for renaming variables later
for(i in 1:length(diff_folders)) {
diff_index_names[i] <- paste0("diff_", substr(diff_folders[i], 19, 30))
}

#grab years present in training data
target_years <- unique(gb_plots$Year)

#set counter equal to one 
counter <- 1

#loop over years present in training data to match with target differenced veg indices
for(j in 1:length(target_years)) {
  #subset training points for a specific year (works with training data for 1 year but adds flexibility in case of multi-year training data)
  gb_plots_subset <- gb_plots[gb_plots$Year == target_years[j],]
  
  #loop over each differenced index, select target year, and extract to points 
  for(i in 1:length(diff_folders)) {
    #list raster files present in particular differenced index folder (same diff index, different years)
    diff_index_sensors <- list.files(diff_folders[i], full.names = T)
    diff_index_sensor <- str_subset(diff_index_sensors, pattern = fixed(sensor_platform))
    diff_index_files <- list.files(diff_index_sensor, full.names = T)
    
    #grab differenced index raster filename for target year
    diff_index_target <- str_subset(diff_index_files, pattern = fixed(target_years[j]))
    
    #load in differenced index as raster object
    diff_index <- raster(diff_index_target)
    
    #set name of raster object using previously created names vector
    names(diff_index) <- diff_index_names[i]
    
    #extract differenced veg index raster values to subset of training points
    gb_plots_subset <- raster::extract(diff_index, gb_plots_subset, sp = T)
    
    #transform subset of training points to lat long and sf object
    gb_plots_subset <- st_transform(st_as_sf(gb_plots_subset), crs = ts_crs)
    
    #put subsets of training data for specific years back together into one data frame
      #if counter = 1 (first year present in training data), create new dataframe object
      #if counter = 2 (other years present in training data), attach subset to dataframe from first iteration of loop 
    if(counter == 1){
      result_diff <- gb_plots_subset
    }else{
      result_diff <- rbind(result, gb_plots_subset)
    }
  }
#advance counter 
counter <- counter + 1
}

result_diff <- result_diff %>% dplyr::mutate(Label = df$Label)
gb_plots <- result_diff
#paths for saving locally and uploading to s3
finished_points_local_filename <- paste0("manual_points_2class_", ts_year, "_ard_ndvi_informed_vars_Jan22.gpkg")
finished_points_local_path <- paste0("data/manual_points_2class_", ts_year, "_ard_ndvi_informed_vars_Jan22.gpkg")
finished_points_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_ndvi_informed/"

#save to local disk
st_write(gb_plots, dsn = finished_points_local_path)

#upload naip points with ALL variables (including differenced indices) to amazon s3 bucket
system(paste0("aws s3 cp ", finished_points_local_path, " ", finished_points_s3_path, finished_points_local_filename))
