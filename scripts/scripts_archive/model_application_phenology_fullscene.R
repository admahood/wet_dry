#Title: Random Forest Model Application LCC Using Differenced Vegetation Index Phenology
#Author(s): Dylan Murphy
#Date Created: July 31, 2019
#Date Last Modified: Aug 7, 2019

#### 1.1: Setup - Load Packages/Source Scripts
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")

#### 1.2: Setup - Create Directories
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

#### 1.3: Setup - Pull Data from S3
s3_ls_path <- "s3://earthlab-amahood/wet_dry/input_raster_data/ls5_mucc"
local_ls_path <- "data/ls5_mucc"
#terrain paths 
s3_terrain <- "s3://earthlab-amahood/wet_dry/input_raster_data/terrain_2"
local_terrain <- "data/terrain_reproj_full"
#precip anomaly raster paths (cropped to NAIP scene already)
s3_precip <- "s3://earthlab-amahood/wet_dry/derived_raster_data/PRISM_precip_anomaly/"
local_precip <- "data/prism/naip_trimmed_annual_precip_anomaly"

#differenced vegetation indices paths
s3_diff <- "s3://earthlab-amahood/wet_dry/derived_raster_data/differenced_indices"
local_diff <- "data/differenced"

#s3 syncs 
system(paste0("aws s3 sync ", s3_ls_path, " ", local_ls_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))
system(paste0("aws s3 sync ", s3_precip, " ", local_precip))
system(paste0("aws s3 sync ", s3_diff, " ", local_diff))

#s3 syncs cont. (masks)
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/landfire_urban_ag_water_mask/ data/urban_ag_mask")

#s3 syncs cont. (naip)
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_raster_data/naip data/naip")

#grab filenames for ls5 stacks 
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)

# IMPORTANT: subset landsat data to just the years where we have differenced indices for now 
scene_full <- scene_full[23:27]

#list differenced veg index files
diff_folders <- list.files("data/differenced/", full.names = T)

counter <- 1
for(i in 1:length(diff_folders)) {
  diff_files <- list.files(diff_folders[i], full.names = T)
  if(counter == 1) {
    diff_files_full <- diff_files 
  } else {
    diff_files_full <- c(diff_files_full, diff_files)
  }
  counter = counter + 1
}

#reproject differenced indices (outside of loop for now)
diff_indices <- stack(diff_files_full) %>% projectRaster(crs = crs, res = 30)

#### 1.4: Setup - Parallelization
cores <- detectCores(all.tests = FALSE, logical = TRUE) / 2
registerDoParallel(cores)

#### 2. Model Application Loop (Parallelized) ####

#get landsat crs
landsat <- stack(scene_full[1])
crs <- crs(landsat)

#create naip scene raster object for cropping 
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") %>% projectRaster(crs = crs, res = 30) #wmuc
naip <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") %>% projectRaster(crs = crs, res = 30) #frank
#naip <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") %>% projectRaster(crs = crs, res = 30) #kings

#parallelized model application loop
foreach(i = scene_full, 
        .packages = 'raster') %dopar% {         
          
          #grab file name without directories for filename creation later 
          file = substr(i, 15, 36) 
          
          #grab year for particular loop iteration
          year = substr(i, 19, 22)
          
          #grab start time for progress check 
          t0 <- Sys.time()
          
          #crop landsat data to naip scene
          # ls5 <- stack(i) %>% crop(naip)
          
          #get diff indices for proper year
          diff_target <- str_subset(diff_files_full, pattern = fixed(year))
          
          
          diff_indices <- stack(diff_target) %>% projectRaster(crs = crs, res = 30)
          
          #crop/reproject differenced indices
          #diff_indices <- diff_indices %>% resample(ls5)
          
          #crop terrain data to naip scene
          ter <- stack(list.files(local_terrain, full.names =T)) %>% resample(diff_indices) 
          
          #get proper names for landsat bands - important for use in veg indice/tassel cap functions later
          #names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
          
          #progress check
          system(paste("echo", "stack created and cropped", i))
          
          #grab precip anomaly for a particular year - change "frank"/"wmuc"/"kings" in both folder and filename depending on which you want to use
          precip <- raster(paste0("data/prism/naip_trimmed_annual_precip_anomaly/greatbasin_trimmed_anomaly_training/precip_anomaly_train", year, ".tif")) %>% resample(diff_indices)
          
          # create additional index variables - make sure all the names of this stack match the names that go into the model 
          # ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          # ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          # ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          # ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
          # ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
          # ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
          # ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
          # ls5$ndsvi <- get_ndsvi(ls5$sr_band3, ls5$sr_band5)
          # ls5$satvi <- get_satvi(ls5$sr_band3, ls5$sr_band5, ls5$sr_band7)
          # system(paste("echo", "veg indices and tassel cap created", i))
          
          #create stack of ls5 data, terrain data, and precip anomaly
          ls5 <- stack(#ls5, 
                       ter, 
                       precip,
                       diff_indices
                       )
          
          #create esp mask and match projection/extent
          esp_mask <- raster("data/esp_binary/clipped_binary.tif")
          esp_mask <- projectRaster(esp_mask, ls5, res = 30)
          system(paste("echo", "esp mask reprojected", i))
          
          #create urban_ag mask and match projection/extent
          urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
          urb_mask <- projectRaster(urb_mask, ls5, res = 30)
          system(paste("echo", "esp mask reprojected", i))
          
          #masking (esp)
          ls5 <- mask(ls5, esp_mask, maskvalue = 0)
          system(paste("echo", "esp masking done", i))
          
          #masking (urb_ag_Water)
          ls5 <- mask(ls5, urb_mask, maskvalue = 1)
          system(paste("echo", "urb ag masking done", i))
          
          # names to match exactly with training data that goes into model. 
          # The order matters for these
          names(ls5) <- c(#"sr_band1", "sr_band2", "sr_band3", "sr_band4", 
                          #"sr_band5", "sr_band7", "wetness", "brightness", 
                          #"greenness",  "ndvi", "savi", "sr", "evi",
                          #"ndsvi", "satvi", 
                          "flowdir", "folded_aspect", "elevation",
                          "roughness", "slope", "tpi", "tri", 
                          "precip_anomaly",
                          "diff_evi", "diff_ndsvi", "diff_ndvi", "diff_satvi",
                          "diff_savi", "diff_sr"
          )
          
          #progress check for stack creation
          system(paste("echo", "stack created"))
          print(Sys.time()-t0)
          
          #for saving memory
          gc() 
          
          #make filename - change "frank"/"wmuc"/"kings" depending on naip scene used for extent
          filenamet <- paste0("data/results/", "ard_all_points_full_scene_", year, "_Aug21", ".tif") 
          system(paste("echo", "filename created", i))
          
          #apply the RF model to raster stack and create "ls5_classed", an annual predicted sage/cheat raster!
          ls5_classed <- raster::predict(ls5, model2, inf.rm = T, na.rm = T) #apply model of choice from list to stack and make predictions
          
          #progress check
          system(paste("echo", "model applied"))
          print(Sys.time()-t0) 
          
          #save resulting land cover rasters and upload to s3
          writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T) 
          system(paste("echo", "file saved to disk"))
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/wet_dry/model_results/summer19_model_results/differenced_variables/ard_full_scene_justdiffs_Aug21/", substr(filenamet, 14, 150)))
          system(paste("echo", "aws sync done"))
          
        }
