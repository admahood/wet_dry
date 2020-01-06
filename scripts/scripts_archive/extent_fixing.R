library(raster)
library(tidyverse)
library(foreach)
library(doParallel)

corz <- detectCores()-1
root_path <- "data/landsat/"
scene_path<- "p42r31"
local_path<-paste0(root_path, scene_path)
s3_path<- "s3://earthlab-amahood/data/ls5_mucc/"
s3_upload_path <- "s3://earthlab-amahood/data/ls5_mucc_extent_matched/"

extent_path<-paste0(root_path,"extents")

system(paste("aws s3 sync ",s3_path, local_path))

l_files <- list.files(local_path, full.names = T)

test<- raster::stack(l_files) #failed!

ee<- extent(raster(l_files[1]))
for (j in 1:length(l_files)){
  r<- raster(l_files[j])
  if(extent(r)@xmin > ee@xmin) ee@xmin <- extent(r)@xmin
  if(extent(r)@xmax < ee@xmax) ee@xmax <- extent(r)@xmax
  if(extent(r)@ymin > ee@ymin) ee@ymin <- extent(r)@ymin
  if(extent(r)@ymax < ee@ymax) ee@ymax <- extent(r)@ymax
}

registerDoParallel(corz)
for(i in 1:length(l_files)){ #each iteration takes about 2 minutes
  t0<- Sys.time()
  
  rr <- raster::stack(l_files[i]) %>%
    crop(ee)
  filename = paste0(substr(l_files[[i]], 0, 35), "_e.tif")
  writeRaster(rr, filename = filename)
  
  system(paste0("aws s3 cp ", filename,  " ", s3_upload_path))
   # by default this only deletes files older than 24 hours(h)
  raster::removeTmpFiles(h=0.001) 
  gc()
  
  system(paste("echo",i, Sys.time()-t0))
  raster::removeTmpFiles(h=0.001) 
  gc()
}

test2 <- raster::stack(l_files) # works!!

#now, create blank raster

blank <- raster(filename) %>% 
  setValues(0) %>%
  writeRaster(filename = paste0(local_path,"/trimmed_extent_",scene_path),
              format = "GTiff") 


#uploading extent fixed ls5 rasters and template raster to s3
system(paste0("aws s3 cp ", local_path,"/trimmed_extent_",scene_path, ".tif"," ", s3_upload_path))

for(i in 1:length(l_files)) { 
  system(paste0("aws s3 cp ", l_files[i], " ", s3_upload_path))
}
