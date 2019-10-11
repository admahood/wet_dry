#Title: Manual Training Data Raster Extraction to Points Using Landsat ARD mean composite band values
#Author(s): Dylan Murphy
#Started: 6/28/19
#Last Modified: 10/9/19

#### 1: Load Packages & Set Up ####
libs <- c("raster", "sf", "dplyr", "stringr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#source scripts
source("/home/rstudio/wet_dry/scripts/functions.R")

#paths 

  #ORIGINAL PIXEL REPLACED LANDSAT PATHS
# landsat_s3 <- "s3://earthlab-amahood/wet_dry/input_raster_data/landsat_5and7_pixel_replaced_Jun3/"
# landsat_local <- "data/landsat_5and7_pixel_replaced_Jun3/" 

  #NEW ARD MEAN COMPOSITE PATHS
landsat_s3 <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites/"
landsat_local <- "data/landsat_ard"
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

#s3 syncs
dir.create("data/training_points/")

system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/manual_training_points/ /home/rstudio/wet_dry/data/training_points")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")

#### 2. create objects and extract path/row combos to each point ####

plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp")


esp_mask <- raster("data/landfire_esp_rcl/clipped_binary.tif")


gb_plots <- st_read("data/training_points/spatially_balanced_points_ard_phenology/ard_pheno_3class_points_final_Sep25.shp") %>% 
  st_transform(as.character(crs(esp_mask))) %>% mutate(ID = row_number(),
                                                       Year = 2010)




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

#grab the years and path row combos needed

years <- unique(gb_plots$Year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row)

#list available pixel replaced landsat files
ls_files_list <- system("aws s3 ls s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites/", intern = T)

#subset ls filenames down to just pathrow, platform, and year
for(l in 1:length(ls_files_list)) {
  ls_files_list[l] <- substr(ls_files_list[l], 32, 51)
}

kounter = 1 #set counter to 1 at beginning of loop 

#### 3. LANDSAT ARD BAND EXTRACTION (SPRING & SUMMER BAND VALUES) ####
#Loop over each year and path/row combo and extract band values to points
for(i in 1:length(years)){ 
  
  #Subset points for a specific year
    gbplots_subset <- gb_plots[gb_plots$Year == years[i],]
    print(paste0("plots subsetted for ", years[i]))
    #print(paste(round(counter/125*100), "%")) #progress indicator
    
    #select proper landsat scene from list of files in s3 bucket
    ls_files_annual <- str_subset(ls_files_list, pattern = fixed(years[i]))
    
    for(j in 1:length(ls_files_annual)) {
    
    season <- substr(ls_files_annual[j], 8, 13)
    #Download proper scene from s3
    system(paste0("aws s3 cp ", landsat_s3, ls_files_annual[j], " ", landsat_local, "/", ls_files_annual[j]))
    print(paste0("landsat downloaded ", years[i], season))    
    
    
    #create raster stack object from newly downloaded landsat scene & set variable names for each band specifying season of mean composite    
    ls_stack <- raster::stack(paste0(landsat_local, "/mc_", years[i], season, ".tif")) # this just loads the raster
    names(ls_stack) <- c(paste0(season, "_sr_band1"), paste0(season, "_sr_band2"), paste0(season, "_sr_band3"), paste0(season, "_sr_band4"), paste0(season, "_sr_band5"), paste0(season, "_sr_band7"))

    #check to make sure there are points for a particular pathrow/year      
     
      
      #extract band values to subset of points
      gbplots_subset <- raster::extract(ls_stack, gbplots_subset, sp=TRUE)
      print(paste0("band values extracted to points ", years[i])) 
      
      #transform points back to lat-long
      gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
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

#### 4. clean up points - remove points with NA extracted values and collapse duplicated points to mean band value ####
  
#removing NA values (points along the outer edge of landsat scene)
result <- result[!is.na(result$spring_sr_band1),] 
result <- result[!is.na(result$summer_sr_band1),] %>% arrange(ID)

#### 5. PRECIP ANOMALY EXTRACTION ####


#change year in path to year of interest for extraction 
system("aws s3 cp s3://earthlab-amahood/wet_dry/derived_raster_data/PRISM_precip_anomaly/greatbasin_trimmed_anomaly_training/precip_anomaly_train2010.tif data/precip_annual/greatbasin_trimmed_anomaly_training/")

result2 <- result %>% mutate(year_factor = as.numeric(as.factor(Year)))
gbd <- result2

training_anomaly_paths <- list.files("data/precip_annual/greatbasin_trimmed_anomaly_training", full.names = T)
#training_anomaly_paths <- training_anomaly_paths[c(5:6)]
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


# attach annual precip anomaly to gb training data 
gbd <- dplyr::mutate(gbd, precip_anomaly = precip_anomaly_vec) %>% dplyr::select(-year_factor)
#### 6. TERRAIN EXTRACTION ####
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/terrain_2 /home/rstudio/wet_dry/data/terrain_2")

df <- gbd

df$elevation <- raster::extract(raster("data/terrain_2/lf_dem_reproj_full.tif"), df)
df$slope <- raster::extract(raster("data/terrain_2/slope.tif"), df)
df$aspect <- raster::extract(raster("data/terrain_2/aspect.tif"), df)
df$tpi <- raster::extract(raster("data/terrain_2/TPI.tif"), df)
df$tri <- raster::extract(raster("data/terrain_2/TRI.tif"), df)
df$roughness <- raster::extract(raster("data/terrain_2/roughness.tif"), df)
df$flowdir <- raster::extract(raster("data/terrain_2/flowdir.tif"), df)

#### 7. VEG INDICES AND TASSELLED CAP VARIABLE CALCULATION ####

  #SPRING
df$spring_ndvi <- get_ndvi(df$spring_sr_band3,df$spring_sr_band4)
df$spring_evi <- get_evi(df$spring_sr_band1, df$spring_sr_band3, df$spring_sr_band4)
df$spring_savi <- get_savi(df$spring_sr_band3,df$spring_sr_band4)
df$spring_sr <- get_sr(df$spring_sr_band3,df$spring_sr_band4)
df$spring_ndti <- get_ndti(band5 = df$spring_sr_band5, band7 = df$spring_sr_band7)
df$spring_green_ndvi <- get_spring_ndvi(band4 = df$spring_sr_band4, band2 = df$spring_sr_band2)
df$spring_sla_index <- get_SLA_index(band3 = df$spring_sr_band3, band4 = df$spring_sr_band4, band7 = df$spring_sr_band7)
df$spring_ndi7 <- get_ndi7(band4 = df$spring_sr_band4, band7 = df$spring_sr_band7)

df$spring_greenness <- green7(df$spring_sr_band1,df$spring_sr_band2,df$spring_sr_band3,df$spring_sr_band4,df$spring_sr_band5,df$spring_sr_band7)
df$spring_brightness <- bright7(df$spring_sr_band1,df$spring_sr_band2,df$spring_sr_band3,df$spring_sr_band4,df$spring_sr_band5,df$spring_sr_band7)
df$spring_wetness <- wet7(df$spring_sr_band1,df$spring_sr_band2,df$spring_sr_band3,df$spring_sr_band4,df$spring_sr_band5,df$spring_sr_band7)

  #SUMMER
df$summer_ndvi <- get_ndvi(df$summer_sr_band3,df$summer_sr_band4)
df$summer_evi <- get_evi(df$summer_sr_band1, df$summer_sr_band3, df$summer_sr_band4)
df$summer_savi <- get_savi(df$summer_sr_band3,df$summer_sr_band4)
df$summer_sr <- get_sr(df$summer_sr_band3,df$summer_sr_band4)
df$summer_ndti <- get_ndti(band5 = df$summer_sr_band5, band7 = df$summer_sr_band7)
df$summer_green_ndvi <- get_summer_ndvi(band4 = df$summer_sr_band4, band2 = df$summer_sr_band2)
df$summer_sla_index <- get_SLA_index(band3 = df$summer_sr_band3, band4 = df$summer_sr_band4, band7 = df$summer_sr_band7)
df$summer_ndi7 <- get_ndi7(band4 = df$summer_sr_band4, band7 = df$summer_sr_band7)

df$summer_greenness <- green7(df$summer_sr_band1,df$summer_sr_band2,df$summer_sr_band3,df$summer_sr_band4,df$summer_sr_band5,df$summer_sr_band7)
df$summer_brightness <- bright7(df$summer_sr_band1,df$summer_sr_band2,df$summer_sr_band3,df$summer_sr_band4,df$summer_sr_band5,df$summer_sr_band7)
df$summer_wetness <- wet7(df$summer_sr_band1,df$summer_sr_band2,df$summer_sr_band3,df$summer_sr_band4,df$summer_sr_band5,df$summer_sr_band7)
#### 8. Saving points with extracted variables (except for differenced indices) and pushing to s3 bucket ####

st_write(df, dsn = "data/manual_points_3class_2010_ard_phenology_nodiffs_Oct9.gpkg")
system("aws s3 cp data/manual_points_3class_2010_ard_phenology_nodiffs_Oct9.gpkg s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/ard_pheno_spatially_balanced_points/manual_points_3class_2010_ard_phenology_nodiffs_Oct9.gpkg")

#### 9. Extracting Differenced Veg. Indices (Phenology Variables) ####

#use old blm data for grabbing longlat crs string
plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp") 

#download manual landsat ARD extent NAIP training points from amazon s3 bucket
system("aws s3 sync s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/ard_pheno_spatially_balanced_points/ data/training_points/")

#create directory to store differenced veg index rasters
dir.create("data/diff_indices")

#download differenced veg indices from amazon s3 bucket
s3_differenced_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices"
system(paste0("aws s3 sync ", s3_differenced_path, " ", "data/diff_indices"))

#load in labelled training points w/ other variables extracted
gb_diff <- st_read("data/training_points/manual_points_3class_2010_ard_phenology_nodiffs_Oct9.gpkg")

#list folders with differenced veg indices
diff_folders <- list.files("data/diff_indices", full.names = T)

#create empty vector to store differenced index names for changing names later on
diff_index_names <- c()

#create vector of differenced index variable names for renaming variables later
for(i in 1:length(diff_folders)) {
diff_index_names[i] <- paste0("diff_", substr(diff_folders[i], 19, 30))
}

#grab years present in training data
target_years <- unique(gb_diff$Year)

#set counter equal to one 
counter <- 1

#loop over years present in training data to match with target differenced veg indices
for(j in 1:length(target_years)) {
  #subset training points for a specific year
  gb_diff_subset <- gb_diff[gb_diff$Year == target_years[j],]
  
  #loop over each differenced index, select target year, and extract to points 
  for(i in 1:length(diff_folders)) {
    #list raster files present in particular differenced index folder (same diff index, different years)
    diff_index_files <- list.files(diff_folders[i], full.names = T)
    
    #grab differenced index raster filename for target year
    diff_index_target <- str_subset(diff_index_files, pattern = fixed(target_years[j]))
    
    #load in differenced index as raster object
    diff_index <- raster(diff_index_target)
    
    #set name of raster object using previously created names vector
    names(diff_index) <- diff_index_names[i]
    
    #extract differenced veg index raster values to subset of training points
    gb_diff_subset <- raster::extract(diff_index, gb_diff_subset, sp = T)
    
    #transform subset of training points to lat long and sf object
    gb_diff_subset <- st_transform(st_as_sf(gb_diff_subset),crs = st_crs(plot_data))
    
    #put subsets of training data for specific years back together into one data frame
      #if counter = 1 (first year present in training data), create new dataframe object
      #if counter = 2 (other years present in training data), attach subset to dataframe from first iteration of loop 
    if(counter == 1){
      result_diff <- gb_diff_subset
    }else{
      result_diff <- rbind(result, gb_diff_subset)
    }
  }
#advance counter 
counter <- counter + 1
}

result_diff <- result_diff %>% dplyr::mutate(Label = df$Label)
#paths for saving locally and uploading to s3
finished_points_local_filename <- "manual_points_2class_2010_ard_phenology_all_variables_extracted_Oct9.gpkg"
finished_points_local_path <- "data/manual_points_2class_2010_ard_phenology_all_variables_extracted_Oct9.gpkg"
finished_points_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_variables_extracted/ard_pheno_spatially_balanced_points/"

#save to local disk
st_write(result_diff, dsn = finished_points_local_path)

#upload naip points with ALL variables (including differenced indices) to amazon s3 bucket
system(paste0("aws s3 cp ", finished_points_local_path, " ", finished_points_s3_path, finished_points_local_filename))

#### 10. Changing manually created Point Labels for different years based on fire history ####

#download manually created NAIP point data from s3
system("aws s3 sync s3://earthlab-amahood/wet_dry/derived_vector_data/manual_training_points_lyb_extracted /home/rstudio/wet_dry/data/training_points")

#read in original (2010) NAIP training data w/ lyb attached
naip_points <- st_read("data/training_points/manual_ard_005007_phenology_points_2010_vars_and_lyb.gpkg") %>% mutate(plot_year = 2010)

#create new object to modify for a new year of labelling (change year in "mutate" to year desired for modeling/labelling)
new_naip_points <- naip_points %>% mutate(plot_year = 2009) %>% 
  filter(plot_year != lyb | is.na(lyb)) #removing points which burned in the year of interest and keeping those that did not burn


#loop through each point and determine whether a steady state can be assumed for the year of choice 
# (because sagebrush takes a long time to establish, even if it burned in between the target year and 
#the year of original labelling (2010) I assume it was sagebrush in the target year)

#remove points labelled grass which burned after the target year - cannot assume they were grass prior to burning
for(i in 1:nrow(new_naip_points)) {
  if(isTRUE(new_naip_points[i,]$lyb > 2009 & new_naip_points[i,]$Label == "grass")) {
  new_naip_points <- new_naip_points[-i,]
  }
}

#remove previously extracted variables from original year 
new_naip_points <- new_naip_points %>% mutate(Year = plot_year) %>% dplyr::select(OBJECTID, Label, Year, lyb)

#create filename for new points (change year to match year of interest)
new_year_naip_filename <- "data/manual_ard_005007_phenology_points_2009_no_vars.gpkg"

st_write(new_naip_points, new_year_naip_filename)

system(paste0("aws s3 cp ", new_year_naip_filename, " s3://earthlab-amahood/wet_dry/input_vector_data/manual_training_points/spatially_balanced_points_ard_phenology/spb_points_ard_w_lyb_novars/", substr(new_year_naip_filename, 6, 100)))

       