# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")
source("scripts/best_randomforest_list_script.R")
# paths -----------------------------------
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

s3_path <- "s3://earthlab-amahood/data/ls5_mucc"
local_path <- "data/ls5_mucc"
s3_terrain <- "s3://earthlab-amahood/data/terrain_2" #(1/25/19)This folder contains the full great basin terrain files reprojected, 
#since the terrain files cropped to the winnemucca scene exactly do not line up every time do to variation in scene position. 
#should be solvable with a buffered terrain stack for each path row in the future 
local_terrain <- "data/terrain_reproj_full"

s3_result <- "s3://earthlab-amahood/data/model_applied_scenes"

#s3 sync -------------------------------- 
system(paste0("aws s3 sync ", s3_path, " ", local_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))

#s3 sync masks
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/data/landfire_urban_ag_water_mask/ data/urban_ag_mask")

#grab filenames for ls5 stacks --------------------------
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)

#setup parallelization ------
cores <- length(scene) / 2 
#cores <- 10 ----Use ten cores to avoid crashing on r4.8xl ec2 instance (244 GB RAM)
registerDoParallel(cores)

#### AS OF JAN 17 THIS IS THE MAIN ALL YEARS MODEL APPLICATION LOOP. USES LOTS OF MEMORY ####
foreach(i = scene_full, 
        .packages = 'raster') %dopar% {         
          file = substr(i, 15, 36) #grab file name without directories for filename creation later 
          t0 <- Sys.time()
          ter <- stack(list.files(local_terrain, full.names =T)) #terrain stack
          
          ls5 <- stack(i) 
          names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
          
          ter_c <- raster::resample(ter, ls5) #resample terrain to match extent, projection, and resolution
          system(paste("echo", "terrain resampled", i))
          
          # create additional index variables - now, make sure all the names of this stack match the names that go into the model and we're golden
          ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
          ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
          ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
          ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
          ls5$ndsvi <- get_ndsvi(ls5$sr_band3, ls5$sr_band5)
          ls5$satvi <- get_satvi(ls5$sr_band3, ls5$sr_band5, ls5$sr_band7)
          system(paste("echo", "veg indices and tassel cap created", i))
          ls5 <- stack(ls5, ter_c)
          
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
          # -- adam-- just name them correctly the first time in the script and delete this
          # -- dylan -- I don't want to delete it because the variable names have changed several times, eg aspect vs folded aspect, and this makes it easier to make sure the right names go to the right layers
          names(ls5) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", 
                          "sr_band5", "sr_band7", "wetness", "brightness", 
                          "greenness",  "ndvi", "savi", "sr", "evi",
                          "ndsvi", "satvi", "flowdir", "folded_aspect", "elevation",
                          "roughness", "slope", "tpi", "tri")
          
          system(paste("echo", "stack created"))
          
          print(Sys.time()-t0)
          gc() # for saving memory
          
          filenamet <- paste0("data/results/", as.character(names(model_list[3])), "_", file) #make filename. change number in brackets to match model used
          system(paste("echo", "filename created", i))
         
          
          ls5_classed <- raster::predict(ls5, model_list[[3]], inf.rm = T, na.rm = T) #apply model of choice from list to stack and make predictions
          
          system(paste("echo", "model applied"))
          
          print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model
          
          writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T) #save prediction raster
          system(paste("echo", "file saved to disk"))
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/mucc_model_results_allyears/", substr(filenamet, 14, 150))) #s3 push prediction raster
          system(paste("echo", "aws sync done"))
          unlink(filenamet)
          
          
          #it appears the task 2 failed error is coming from the deletion of temp files. I commented it out for now - Dylan
          #system("rm data/tmp/*") # so we're not filling up the hard drive (had to move this to the end because the model needs the stuff stored in temp directory - D)
        }
  

# # Everything Below here was for the single year model application script but is kept for reference ------------
# # whole lotta stuff that takes a long time doesn't need to be parallelized
# # ls5_e <- stack(scene_full[i])
# # ter_c <-raster::resample(ter, ls5_e)
# 
# #create terrain raster stack
# #ter <- stack(list.files(local_terrain, full.names =T))
# #ter_c <-raster::resample(ter, ls5_p)
# # create esp mask object and match projection/resolution 
# ls5_p <- stack(scene_full[1])
# esp_mask <- raster("data/esp_binary/clipped_binary.tif")
# esp_mask <- projectRaster(esp_mask, ls5_p, res = 30)
# 
# #create urban_ag mask and match projection/extent
# urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
# urb_mask <- projectRaster(urb_mask, ls5_p, res = 30)
# 
# #only parallelize this part
# foreach(i = scene_full, 
#         .packages = 'raster') %dopar% {         
#           file = substr(i, 15, 36)
#           t0 <- Sys.time()
#           ter<- stack(list.files(local_terrain, full.names =T))
#           
#           ls5 <- stack(i)
#           names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
#           
#           ter_c <- raster::resample(ter, ls5)
#           system(paste("echo", "terrain resampled"))
#           # ne <- projectExtent(ls5, crs=crs(ter_p))
#           # res(ne) <- 30
#           # 
#           # ter_files <- list.files("data/terrain", full.names = T) 
#           # ter_names <- list.files("data/terrain", full.names = F)
#           # dir.create(paste0("data/terrain_reproj", substr(i, 19, 22)))
#           
#           # for(l in 1:length(ter_files)) {   #this loop uses up to 45GB of ram per core, will need to improve efficiency when scaling up. -Dylan
#           #   
#           #   
#           #   ter <- raster(ter_files[l])
#           #   print(ter_names[l])
#           #   
#           #   ter <- crop(ter, ne)
#           #   print(paste(ter_names[l], "cropped"))
#           #   
#           #   ter <- projectRaster(ter, crs = crs(ls5), res = 30, filename = paste0("data/terrain_reproj", substr(i, 19, 22), "/", "reproj_", ter_names[l]))
#           #   print(paste(ter_names[l], "projected"))
#           #   
#           #   ter <- resample(ter, ls5, filename = paste0("data/terrain_reproj", substr(i, 19, 22), "/", "reproj_", ter_names[l]), overwrite = T)
#           #   print(paste(ter_names[l], "resampled"))
#           #   
#           #   print(paste(ter_names[l], "done"))
#           # }
#           # 
#           # ter_c <- stack(list.files(paste0("data/terrain_reproj", substr(i, 19, 22)), full.names = T))
#           # 
#           
#           # ter 
#           # ter <- crop(ne)
#           # ter_c <- projectRaster(ter, crs = crs(ls5), res = 30)
#           # ter_c <- resample(ter, ls5)
#           
#           
#           # now, make sure all the names of this stack match the names that go into the model and we're golden
#           ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
#           ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
#           ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
#           ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
#           ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
#           ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
#           ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
#           ls5$ndsvi <- get_ndsvi(ls5$sr_band3, ls5$sr_band5)
#           ls5$satvi <- get_satvi(ls5$sr_band3, ls5$sr_band5, ls5$sr_band7)
#           
#           ls5 <- stack(ls5, ter_c)
#           ls5 <- mask(ls5, esp_mask, maskvalue = 0)
#           ls5 <- mask(ls5, urb_mask, maskvalue = 1)
#           
#           names(ls5) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", 
#                           "sr_band5", "sr_band7", "wetness", "brightness", 
#                           "greenness",  "ndvi", "savi", "sr", "evi",
#                           "ndsvi", "satvi", "flowdir", "folded_aspect", "elevation",
#                           "roughness", "slope", "tpi", "tri")
#           # names to match exactly with training data that goes into model. 
#           # The order matters for these
#           
#           system(paste("echo", "stack created"))
#           
#           print(Sys.time()-t0)
#           gc() # for saving memory
#           
#           for(j in 1:length(model_list)) {
#             
#             filenamet <- paste0("data/results/", names(model_list[j]), "_", 
#                                 file) 
#             # frst <- model_list[[i]]
#             # now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file) 
#             ls5_classed <- raster::predict(ls5, model_list[[j]], inf.rm = T, na.rm = T)
#             
#             system(paste("echo", "model applied", as.character(j)))
#             
#             # matrix <- c(1, 0, 2, 1)
#             # rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
#             # ls5_classed <- reclassify(ls5_classed, rclss_matrix)
#             
#             print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model
#             
#             writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T)
#             system(paste("echo", "file created"))
#             system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/mucc_model_results_allyears/", substr(filenamet, 14, 150)))
#             system(paste("echo", "aws sync done"))
#             unlink(filenamet)
#             
#           }
#           #it appears the task 2 failed error is coming from the deletion of temp files. I commented it out for now - Dylan
#           #system("rm data/tmp/*") # so we're not filling up the hard drive (had to move this to the end because the model needs the stuff stored in temp directory - D)
#         }
# 
