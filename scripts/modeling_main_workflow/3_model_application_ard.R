#Title: Random Forest Model Application LCC Using Differenced Vegetation Index Phenology
#Author(s): Dylan Murphy
#Date Created: July 31, 2019
#Date Last Modified: Dec 17, 2019

#### 1.1: Setup - Load Packages/Source Scripts
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils", "stringr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")

#### 1.2: Setup - Create Directories
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

#### 1.3: Setup - Pull Data from S3
s3_ls_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites_ndvi"
local_ls_path <- "data/mean_composites"

#terrain paths 
s3_terrain <- "s3://earthlab-amahood/wet_dry/input_raster_data/terrain_2"
local_terrain <- "data/terrain_reproj_full"

#precip anomaly raster paths (cropped to NAIP scene already)
s3_precip <- "s3://earthlab-amahood/wet_dry/derived_raster_data/PRISM_precip_anomaly/naip_trimmed_annual_precip_anomaly"
local_precip <- "data/prism/naip_trimmed_annual_precip_anomaly"

#differenced vegetation indices paths
s3_diff <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices"
local_diff <- "data/differenced"

#climate zscore paths: 
s3_zscore <- "s3://earthlab-amahood/wet_dry/input_raster_data/climate_zscores"
local_zscore <- "data/climate"

#actual precip paths (monthly, cropped): 
s3_monthly_precip <- "s3://earthlab-amahood/wet_dry/input_raster_data/PRISM_precip/monthly_precip_resample"
local_monthly_precip <- "data/prism/monthly_precip"

#actual precip paths (seasonal, cropped): 
s3_seasonal_precip <- "s3://earthlab-amahood/wet_dry/input_raster_data/PRISM_precip/PRISM_seasonal_precip"
local_seasonal_precip <- "data/prism/seasonal_precip"
  
  
#s3 syncs 
system(paste0("aws s3 sync ", s3_ls_path, " ", local_ls_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))
system(paste0("aws s3 sync ", s3_precip, " ", local_precip))
system(paste0("aws s3 sync ", s3_diff, " ", local_diff))
system(paste0("aws s3 sync ", s3_monthly_precip, " ", local_monthly_precip))
system(paste0("aws s3 sync ", s3_seasonal_precip, " ", local_seasonal_precip))
system(paste0("aws s3 sync ", s3_zscore, " ", local_zscore))

#s3 syncs cont. (masks)
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_urban_ag_water_mask/ data/urban_ag_mask")

#s3 syncs cont. (naip)
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/naip data/naip")

#grab filenames for ls5 stacks 
scene_folders <- list.files("data/mean_composites", full.names = T)
scene <- list.files(str_subset(scene_folders, pattern = fixed("ls5_and_ls7")))
scene_full <- list.files(str_subset(scene_folders, pattern = fixed("ls5_and_ls7")), full.names = T)

spring_scenes <- str_subset(scene_full, pattern = fixed("spring"))
summer_scenes <- str_subset(scene_full, pattern = fixed("summer"))

#list differenced veg index files
diff_folders <- list.files("data/differenced/", full.names = T)

counter <- 1
for(i in 1:length(diff_folders)) {
  diff_platform_folders <- list.files(diff_folders[i], full.names = T)
  if(counter == 1) {
    diff_platforl_folders_full <- diff_files 
  } else {
    diff_platform_folders_full <- c(diff_files_full, diff_files)
  }
  counter = counter + 1
}

counter <- 1
for(i in 1:length(diff_platform_folders_full)) {
  diff_files <- list.files(diff_platform_folders_full[i], full.names = T)
  if(counter == 1) {
    diff_files_full <- diff_files 
  } else {
    diff_files_full <- c(diff_files_full, diff_files)
  }
  counter = counter + 1
}
#get folder locations for monthly and seasonal actual precip rasters
monthly_precip_folders <- list.files("data/prism/monthly_precip/cropped", full.names = T)

seasonal_precip_folders <- list.files("data/prism/seasonal_precip", full.names = T)

#get landsat crs
landsat <- stack(scene_full[1])
crs <- crs(landsat)

#create directory to store model predictions
dir.create("data/results")

#### 1.4: Setup - Parallelization
cores <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoParallel(cores)

#### 2. Model Application Loop (Parallelized) ####

#create naip scene raster object for cropping 
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") %>% projectRaster(crs = crs, res = 30) #wmuc
#naip <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") %>% projectRaster(crs = crs, res = 30) #frank
#naip <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") %>% projectRaster(crs = crs, res = 30) #kings

#create object to store name of naip scene being modeled while testing model results

#IMPORTANT: change to "wmuc", "frank", or "kings"
naip_name <- "frank"


#parallelized model application loop
foreach(i = spring_scenes, 
        .packages = 'raster') %dopar% {         
          
      #grab file name without directories for filename creation later 
          file = substr(i, 34, 70) 
          
      #grab year for particular loop iteration
          year = substr(i, 37, 40)
          
      #grab start time for progress check 
          t0 <- Sys.time()
          
      #crop spring/summer landsat data to naip scene
          ard_spring <- stack(str_subset(spring_scenes, pattern = fixed(year))) %>% resample(naip)
          ard_summer <- stack(str_subset(summer_scenes, pattern = fixed(year))) %>% resample(naip)
          
      #progress check
          system(paste("echo", "seasonal ard stacked", year))
          
      #crop terrain data to naip scene
          ter <- stack(list.files(local_terrain, full.names =T)) %>% projectRaster(to = naip)
          
      #progress check
          system(paste("echo", "terrain stacked", year))
          
      #get proper names for landsat bands - important for use in veg indice/tassel cap functions later
          names(ard_spring)<- c("spring_sr_band1", "spring_sr_band2","spring_sr_band3", "spring_sr_band4","spring_sr_band5", "spring_sr_band7")
          names(ard_summer)<- c("summer_sr_band1", "summer_sr_band2","summer_sr_band3", "summer_sr_band4","summer_sr_band5", "summer_sr_band7")
          
          ard <- stack(ard_spring, ard_summer)
      #progress check
          system(paste("echo", "stack created and cropped", year))
          
      #grab precip anomaly for a particular year - change "frank"/"wmuc"/"kings" in both folder and filename depending on which you want to use
          precip_anom <- raster(paste0("data/prism/naip_trimmed_annual_precip_anomaly/", naip_name, "/precip_anomaly_trimmed_", naip_name, "_", year, ".tif")) %>% projectRaster(crs = crs) %>% resample(ard)
          
      #progress check
          system(paste("echo", "precip anom grabbed", year))
          
      #grab actual precip variables (monthly & seasonal)
          
      #MONTHLY
          monthly_precip_folder <- str_subset(monthly_precip_folders, pattern = fixed(naip_name))
          monthly_precip_files <- list.files(monthly_precip_folder, full.names = T)
          
          monthly_precip_targets_sameyear <- str_subset(monthly_precip_files, pattern = fixed(year))
          monthly_precip_targets_sameyear <- monthly_precip_targets_sameyear[1:8] #subset to jan-aug
          monthly_precip_sameyear <- stack(monthly_precip_targets_sameyear) %>% projectRaster(to = ard)
          
          monthly_precip_targets_prioryear <- str_subset(monthly_precip_files, pattern = fixed(as.character(as.numeric(year) - 1)))
          monthly_precip_targets_prioryear <- monthly_precip_targets_prioryear[9:12] #subset to sept-dec
          monthly_precip_prioryear <- stack(monthly_precip_targets_prioryear) %>% projectRaster(to = ard)
          
          monthly_precip <- stack(monthly_precip_sameyear, monthly_precip_prioryear)
          
      #progress check
          system(paste("echo", "monthly precip grabbed", year))
          
      #SEASONAL
          #spring
          spring_precip_files <- list.files(seasonal_precip_folders[2], full.names = T)
          spring_precip <- str_subset(spring_precip_files, pattern = fixed(year)) %>% 
            str_subset(pattern = fixed(naip_name)) %>% 
            raster()
          
          
          #summer
          summer_precip_files <- list.files(seasonal_precip_folders[3], full.names = T)
          summer_precip <- str_subset(summer_precip_files, pattern = fixed(year)) %>% 
            str_subset(pattern = fixed(naip_name)) %>% 
            raster()
          
          #fall
          fall_precip_files <- list.files(seasonal_precip_folders[1], full.names = T)
          fall_precip <- str_subset(fall_precip_files, pattern = fixed(year)) %>% 
            str_subset(pattern = fixed(naip_name)) %>% 
            raster()
          
          #winter
          winter_precip_files <- list.files(seasonal_precip_folders[4], full.names = T)
          winter_precip <- str_subset(winter_precip_files, pattern = fixed(year)) %>% 
            str_subset(pattern = fixed(naip_name)) %>% 
            raster()
          
          seasonal_precip <- stack(winter_precip, spring_precip, summer_precip, fall_precip) %>% projectRaster(to = ard)
          
          #progress check
          system(paste("echo", "seasonal precip grabbed", year))
          
       #CLIMATE Z SCORES
          
          zscore_files <- list.files("data/climate", full.names = T)
          
          zscore_vars <- list()
            prior_year <- as.numeric(year) - 1
            target_zscore_paths_sameyear <- str_subset(zscore_files, pattern = fixed(year))
            target_zscore_paths_prioryear <- str_subset(zscore_files, pattern = fixed(as.character(prior_year)))
            for(j in 1:length(target_zscore_paths_sameyear)) {
              zscore_layer_sameyear <- stack(target_zscore_paths_sameyear[j])
              zscore_layer_prior_year <- stack(target_zscore_paths_prioryear[j])
              #change the below subsets to grab different months for "water year"
              zscore_layer_water_year <- stack(zscore_layer_prior_year[[9:12]], zscore_layer_sameyear[[1:3]]) 
              zscore_layer <- mean(zscore_layer_water_year[[1:7]]) #change if water year months != 7
              zscore_layer <- zscore_layer %>% projectRaster(to = ard)
              zscore_vars[[j]] <- zscore_layer
            }
          
          zscore_vars <- stack(zscore_vars)
          names(zscore_vars) <- c("aet_z", "def_z", "tmn_z")
          
      #progress check for stack creation
          system(paste("echo", "z score rasters for water year created and stacked ", year))
          print(Sys.time()-t0)
          
      # create additional index variables - make sure all the names of this stack match the names that go into the model 
      #SPRING
          ard$spring_wetness <- wet5(ard$spring_sr_band1,ard$spring_sr_band2,ard$spring_sr_band3,ard$spring_sr_band4,ard$spring_sr_band5,ard$spring_sr_band7)
          ard$spring_brightness <- bright5(ard$spring_sr_band1,ard$spring_sr_band2,ard$spring_sr_band3,ard$spring_sr_band4,ard$spring_sr_band5,ard$spring_sr_band7)
          ard$spring_greenness <- green5(ard$spring_sr_band1,ard$spring_sr_band2,ard$spring_sr_band3,ard$spring_sr_band4,ard$spring_sr_band5,ard$spring_sr_band7)
          ard$spring_ndvi <- get_ndvi(ard$spring_sr_band3, ard$spring_sr_band4)
          ard$spring_savi <- get_savi(ard$spring_sr_band3, ard$spring_sr_band4)
          ard$spring_sr <- get_sr(ard$spring_sr_band3, ard$spring_sr_band4)
          ard$spring_evi <- get_evi(ard$spring_sr_band1, ard$spring_sr_band3, ard$spring_sr_band4)
          ard$spring_ndsvi <- get_ndsvi(ard$spring_sr_band3, ard$spring_sr_band5)
          ard$spring_satvi <- get_satvi(ard$spring_sr_band3, ard$spring_sr_band5, ard$spring_sr_band7)
          ard$spring_ndti <- get_ndti(band5 = ard$spring_sr_band5, band7 = ard$spring_sr_band7)
          ard$spring_green_ndvi <- get_green_ndvi(band4 = ard$spring_sr_band4, band2 = ard$spring_sr_band2)
          ard$spring_sla_index <- get_SLA_index(band3 = ard$spring_sr_band3, band4 = ard$spring_sr_band4, band7 = ard$spring_sr_band7)
          ard$spring_ndi7 <- get_ndi7(band4 = ard$spring_sr_band4, band7 = ard$spring_sr_band7)
          
      #SUMMER
          ard$summer_wetness <- wet5(ard$summer_sr_band1,ard$summer_sr_band2,ard$summer_sr_band3,ard$summer_sr_band4,ard$summer_sr_band5,ard$summer_sr_band7)
          ard$summer_brightness <- bright5(ard$summer_sr_band1,ard$summer_sr_band2,ard$summer_sr_band3,ard$summer_sr_band4,ard$summer_sr_band5,ard$summer_sr_band7)
          ard$summer_greenness <- green5(ard$summer_sr_band1,ard$summer_sr_band2,ard$summer_sr_band3,ard$summer_sr_band4,ard$summer_sr_band5,ard$summer_sr_band7)
          ard$summer_ndvi <- get_ndvi(ard$summer_sr_band3, ard$summer_sr_band4)
          ard$summer_savi <- get_savi(ard$summer_sr_band3, ard$summer_sr_band4)
          ard$summer_sr <- get_sr(ard$summer_sr_band3, ard$summer_sr_band4)
          ard$summer_evi <- get_evi(ard$summer_sr_band1, ard$summer_sr_band3, ard$summer_sr_band4)
          ard$summer_ndsvi <- get_ndsvi(ard$summer_sr_band3, ard$summer_sr_band5)
          ard$summer_satvi <- get_satvi(ard$summer_sr_band3, ard$summer_sr_band5, ard$summer_sr_band7)
          ard$summer_ndti <- get_ndti(band5 = ard$summer_sr_band5, band7 = ard$summer_sr_band7)
          ard$summer_green_ndvi <- get_green_ndvi(band4 = ard$summer_sr_band4, band2 = ard$summer_sr_band2)
          ard$summer_sla_index <- get_SLA_index(band3 = ard$summer_sr_band3, band4 = ard$summer_sr_band4, band7 = ard$summer_sr_band7)
          ard$summer_ndi7 <- get_ndi7(band4 = ard$summer_sr_band4, band7 = ard$summer_sr_band7)
          
          system(paste("echo", "veg indices and tassel cap created", year))
          
          diff_target <- str_subset(diff_files_full, pattern = fixed(year))
          
          
          diff_indices <- stack(diff_target) %>% projectRaster(to = ard)
          
      #crop/reproject differenced indices
          diff_indices <- diff_indices %>% crop(naip) %>% resample(ard)
          
      #create stack of ls5 data, terrain data, and precip anomaly
          ard <- stack(ard, 
                       ter, 
                       precip_anom,
                       diff_indices,
                       monthly_precip,
                       seasonal_precip, 
                       zscore_vars)
          
      #progress check
          system(paste("echo", "ard, precip_anom, terrain, differenced indices, monthly and seasonal precip stacked together", year))
          
      #create esp mask and match projection/extent
          esp_mask <- raster("data/esp_binary/clipped_binary.tif")
          esp_mask <- projectRaster(esp_mask, ard, res = 30)
          system(paste("echo", "esp mask reprojected", year))
          
      #create urban_ag mask and match projection/extent
          urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
          urb_mask <- projectRaster(urb_mask, ard, res = 30)
          system(paste("echo", "urb_ag mask reprojected", year))
          
      #masking (esp)
          ard <- mask(ard, esp_mask, maskvalue = 0)
          system(paste("echo", "esp masking done", year))
          
      #masking (urb_ag_Water)
          ard <- mask(ard, urb_mask, maskvalue = 1)
          system(paste("echo", "urb ag masking done", year))
          
      # names to match exactly with training data that goes into model. 
          # The order matters for these (must match order of layers in stack)
          names(ard) <- c("spring_sr_band1", "spring_sr_band2", "spring_sr_band3", "spring_sr_band4", 
                          "spring_sr_band5", "spring_sr_band7", 
                          "summer_sr_band1", "summer_sr_band2", "summer_sr_band3", "summer_sr_band4", 
                          "summer_sr_band5", "summer_sr_band7",
                          "spring_wetness", "spring_brightness", 
                          "spring_greenness",  "spring_ndvi", "spring_savi", "spring_sr", "spring_evi",
                          "spring_ndsvi", "spring_satvi", 
                          "spring_ndti", "spring_green_ndvi", "spring_sla_index", "spring_ndi7",
                          "summer_wetness", "summer_brightness", 
                          "summer_greenness",  "summer_ndvi", "summer_savi", "summer_sr", "summer_evi",
                          "summer_ndsvi", "summer_satvi", 
                          "summer_ndti", "summer_green_ndvi", "summer_sla_index", "summer_ndi7",
                          "flowdir", "folded_aspect", "elevation",
                          "roughness", "slope", "tpi", "tri", 
                          "precip_anomaly",
                          "diff_evi", "diff_green_ndvi", "diff_ndi7", "diff_ndsvi", 
                          "diff_ndti", "diff_ndvi", "diff_satvi",
                          "diff_savi", "diff_sla_index", "diff_sr", 
                          "jan_precip", "feb_precip", "mar_precip", "apr_precip", "may_precip",
                          "jun_precip", "jul_precip", "aug_precip", "sep_precip", "oct_precip", "nov_precip", "dec_precip",
                          "winter_precip", "spring_precip", "summer_precip", "fall_precip",
                          "aet_z", "def_z", "tmn_z"
          )
          
      #progress check for stack creation
          system(paste("echo", "full stack created and names set", year))
          print(Sys.time()-t0)
          
      #for saving memory
          gc() 
          
      #make filename 
          filenamet <- paste0("data/results/", "005007_2class_ndvi_informed_indices_", naip_name, "_", year, "_Jan24", ".tif") 
          filename_short <-  paste0("data/results/", "005007_2class_ndvi_informed_indices_", naip_name, "_", year, "_Jan24", ".tif") 
          system(paste("echo", "filename created", year))
          
      #apply the RF model to raster stack and create "ls5_classed", an annual predicted sage/cheat raster!
          ls5_classed <- raster::predict(ard, model2, inf.rm = T, na.rm = T) #apply model of choice from list to stack and make predictions
          
      #progress check
          system(paste("echo", "model applied"))
          print(Sys.time()-t0) 
          
      #save resulting land cover rasters and upload to s3
          writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T) 
          system(paste("echo", "file saved to disk"))
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/wet_dry/model_results/summer19_model_results/differenced_variables/2class_ndvi_informed_indicesJan24/", naip_name, "/", filename_short))
          system(paste("echo", "aws sync done"))
          
        }
