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

s3_path <- "s3://earthlab-amahood/data/ls5_mucc_2011"
local_path <- "data/ls5_mucc"
s3_terrain <- "s3://earthlab-amahood/data/terrain_mucc"
local_terrain <- "data/terrain_2"

s3_result <- "s3://earthlab-amahood/data/model_applied_scenes"

#s3 sync -------------------------------- 
system(paste0("aws s3 sync ", s3_path, " ", local_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))
#s3 sync masks
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/data/landfire_urban_ag_water_mask/ data/urban_ag_mask")
# system(paste0("aws s3 sync ", 
# " s3://earthlab-amahood/data/ls5_model_results_test_mucc/", 
# "data/results")) ## figure out how to use an if statement based on this s3 sync
##to avoid redundant classified images

#cropping and stacking and applying the model loop --------------------------
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)

cores <- length(model_list)

registerDoParallel(cores)

# whole lotta stuff that takes a long time doesn't need to be parallelized

file <- as.character(scene)
t0 <- Sys.time()
ter <- stack(list.files(local_terrain, full.names =T))
ls5 <- stack(scene_full)
names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")

# create esp mask object and match projection/extent 

esp_mask <- raster("data/esp_binary/clipped_binary.tif")
esp_mask <- projectRaster(esp_mask, ls5, res = 30)

#create urban_ag mask and match projection/extent
urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
urb_mask <- projectRaster(urb_mask, ls5, res = 30)

ter_c <-raster::resample(ter,ls5)
system(paste("echo", "terrain cropped"))
# now, make sure all the names of this stack match the names that go into the model and we're golden
ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
#ls5$satvi <- get_satvi(ls5$sr_band3, ls5$sr_band5, ls5$sr_band7)
ls5$ndsvi <- get_ndsvi(ls5$sr_band3, ls5$sr_band5)

ls5 <- stack(ls5, ter_c)
ls5 <- mask(ls5, esp_mask, maskvalue = 0)
ls5 <- mask(ls5, urb_mask, maskvalue = 1)

names(ls5) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", 
                "sr_band5", "sr_band7", "wetness", "brightness", 
                "greenness",  "ndvi", "savi", "sr", "evi",  #"satvi",
                "ndsvi", "elevation", "flowdir", "folded_aspect",
                "roughness", "slope", "tpi", "tri")
# names to match exactly with training data that goes into model. 
# The order matters for these

system(paste("echo", "stack created"))

print(Sys.time()-t0)
gc() # for saving memory

#only parallelize this part
foreach(i = 1:length(model_list), 
        .packages = 'raster') %dopar% {         
          filenamet <- paste0("data/results/", "nobuff_", as.character(names(model_list[i])), "_",
                              sub(pattern = ".tif", 
                                  replacement = "model_results.tif", 
                                  x = file, fixed = T)) 
          # frst <- model_list[[i]]
          # now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file) 
          ls5_classed <- raster::predict(ls5, model_list[[i]], inf.rm = T, na.rm = T)
          
          system(paste("echo", "model applied"))
          
          
          print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model
          
          writeRaster(ls5_classed, filename = filenamet, overwrite = T, progress = 'text')
          
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/ls5_hyperparameter_test_results/", substr(filenamet, 14, 100)))
          system(paste("echo", "done"))
          unlink(filenamet)
          
          #it appears the task 2 failed error is coming from the deletion of temp files. I commented it out for now - Dylan
          #system("rm data/tmp/*") # so we're not filling up the hard drive (had to move this to the end because the model needs the stuff stored in temp directory - D)
        }


#making a smaller stack for troubleshooting possible RAM issues
e = extent(ls5)
f = e
f@xmin = f@xmin + 160500
f@xmax = f@xmax - 80500
f@ymin = f@ymin + 151000
f@ymax = f@ymax -66250
plot(f)
ls5_small = crop(ls5, f)
plot(ls5_small[[1]])

#single iteration of model apply foreach loop ---------
filenamet <- paste0("data/results/", as.character(names(model_list[i])), "_",
                    sub(pattern = ".tif", 
                        replacement = "model_results.tif", 
                        x = file, fixed = T)) 
# frst <- model_list[[i]]
# now put a line to apply the model and write THAT as the raster and send it to s3 (and then delete the file) 
ls5_classed <- predict(object = ls5_small, frst, type = 'class', inf.rm = T, na.rm = T)

system(paste("echo", "model applied"))


print(Sys.time()-t0) #checking elapsed time between creation of big stack and application of model

writeRaster(ls5_classed, filename = filenamet, overwrite = T, progress = 'text')

system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/ls5_mucc_2011/", substr(filenamet, 14, 63)))
system(paste("echo", "done"))
unlink(filenamet)

