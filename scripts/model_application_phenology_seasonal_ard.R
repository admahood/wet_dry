#Title: Random Forest Model Application LCC Using Differenced Vegetation Index Phenology
#Author(s): Dylan Murphy
#Date Created: July 31, 2019
#Date Last Modified: Oct 10, 2019

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
s3_ls_path <- "s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites"
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
scene <- list.files("data/mean_composites")
scene_full <- list.files("data/mean_composites", full.names = T)



spring_scenes <- str_subset(scene_full, pattern = fixed("spring"))
summer_scenes <- str_subset(scene_full, pattern = fixed("summer"))

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

#get landsat crs
landsat <- stack(scene_full[1])
crs <- crs(landsat)

#reproject differenced indices (outside of loop for now)
diff_indices <- stack(diff_files_full) %>% projectRaster(crs = crs, res = 30)

#### 1.4: Setup - Parallelization
cores <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoParallel(cores)

#### 2. Model Application Loop (Parallelized) ####

#create naip scene raster object for cropping 
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") %>% projectRaster(crs = crs, res = 30) #wmuc
#naip <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") %>% projectRaster(crs = crs, res = 30) #frank
#naip <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") %>% projectRaster(crs = crs, res = 30) #kings

#parallelized model application loop
foreach(i = spring_scenes, 
        .packages = 'raster') %dopar% {         
          
          #grab file name without directories for filename creation later 
          file = substr(i, 22, 45) 
          
          #grab year for particular loop iteration
          year = substr(i, 25, 28)
          
          #grab start time for progress check 
          t0 <- Sys.time()
          
          #crop spring/summerlandsat data to naip scene
          ard_spring <- stack(str_subset(spring_scenes, pattern = fixed(year))) %>% crop(naip)
          ard_summer <- stack(str_subset(summer_scenes, pattern = fixed(year))) %>% crop(naip)
          
          #progress check
          system(paste("echo", "seasonal ard stacked", year))
          
          #crop terrain data to naip scene
          ter <- stack(list.files(local_terrain, full.names =T))
          naip <- projectRaster(naip, crs = crs(ter))
          
          ter <- ter %>% crop(naip) %>% projectRaster(crs = crs) %>% resample(ard_spring) 
          naip <- projectRaster(naip, crs = crs)
          
          #progress check
          system(paste("echo", "terrain stacked", year))
          
          #get proper names for landsat bands - important for use in veg indice/tassel cap functions later
          names(ard_spring)<- c("spring_sr_band1", "spring_sr_band2","spring_sr_band3", "spring_sr_band4","spring_sr_band5", "spring_sr_band7")
          names(ard_summer)<- c("summer_sr_band1", "summer_sr_band2","summer_sr_band3", "summer_sr_band4","summer_sr_band5", "summer_sr_band7")
          
          ard <- stack(ard_spring, ard_summer)
          #progress check
          system(paste("echo", "stack created and cropped", year))
          
          #grab precip anomaly for a particular year - change "frank"/"wmuc"/"kings" in both folder and filename depending on which you want to use
          precip <- raster(paste0("data/prism/naip_trimmed_annual_precip_anomaly/frank/precip_anomaly_trimmed_frank_", year, ".tif")) %>% projectRaster(crs = crs) %>% resample(ard)
          
          #progress check
          system(paste("echo", "precip grabbed", year))
          
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
          
          system(paste("echo", "veg indices and tassel cap created", year))
          
          diff_target <- str_subset(diff_files_full, pattern = fixed(year))
          
          
          diff_indices <- stack(diff_target) %>% projectRaster(crs = crs, res = 30)
          
          #crop/reproject differenced indices
          diff_indices <- diff_indices %>% crop(naip) %>% resample(ard)
          
          #create stack of ls5 data, terrain data, and precip anomaly
          ard <- stack(ard, 
                       ter, 
                       precip,
                       diff_indices)
          
          #progress check
          system(paste("echo", "ard, precip, terrain stacked together", year))
          
          #create esp mask and match projection/extent
          esp_mask <- raster("data/esp_binary/clipped_binary.tif")
          esp_mask <- projectRaster(esp_mask, ard, res = 30)
          system(paste("echo", "esp mask reprojected", year))
          
          #create urban_ag mask and match projection/extent
          urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
          urb_mask <- projectRaster(urb_mask, ard, res = 30)
          system(paste("echo", "esp mask reprojected", year))
          
          #masking (esp)
          ard <- mask(ard, esp_mask, maskvalue = 0)
          system(paste("echo", "esp masking done", year))
          
          #masking (urb_ag_Water)
          ard <- mask(ard, urb_mask, maskvalue = 1)
          system(paste("echo", "urb ag masking done", year))
          
          # names to match exactly with training data that goes into model. 
          # The order matters for these
          names(ard) <- c("spring_sr_band1", "spring_sr_band2", "spring_sr_band3", "spring_sr_band4", 
                          "spring_sr_band5", "spring_sr_band7", 
                          "summer_sr_band1", "summer_sr_band2", "summer_sr_band3", "summer_sr_band4", 
                          "summer_sr_band5", "summer_sr_band7",
                          "spring_wetness", "spring_brightness", 
                          "spring_greenness",  "spring_ndvi", "spring_savi", "spring_sr", "spring_evi",
                          "spring_ndsvi", "spring_satvi", 
                          "summer_wetness", "summer_brightness", 
                          "summer_greenness",  "summer_ndvi", "summer_savi", "summer_sr", "summer_evi",
                          "summer_ndsvi", "summer_satvi", 
                          "flowdir", "folded_aspect", "elevation",
                          "roughness", "slope", "tpi", "tri", 
                          "precip_anomaly",
                          "diff_evi", "diff_ndsvi", "diff_ndvi", "diff_satvi",
                          "diff_savi", "diff_sr"
          )
          
          #progress check for stack creation
          system(paste("echo", "full stack created and names set", year))
          print(Sys.time()-t0)
          
          #for saving memory
          gc() 
          
          #make filename - change "frank"/"wmuc"/"kings" depending on naip scene used for extent
          filenamet <- paste0("data/results/", "005007_2class_spring_summer_allvars", "_frank_", year, "_Oct10", ".tif") 
          system(paste("echo", "filename created", year))
          
          #apply the RF model to raster stack and create "ls5_classed", an annual predicted sage/cheat raster!
          ls5_classed <- raster::predict(ard, model2, inf.rm = T, na.rm = T) #apply model of choice from list to stack and make predictions
          
          #progress check
          system(paste("echo", "model applied"))
          print(Sys.time()-t0) 
          
          #save resulting land cover rasters and upload to s3
          writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T) 
          system(paste("echo", "file saved to disk"))
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/wet_dry/model_results/summer19_model_results/differenced_variables/2class_ard_spring_summer_allvars_Oct10/frank/", substr(filenamet, 14, 150)))
          system(paste("echo", "aws sync done"))
          
        }
