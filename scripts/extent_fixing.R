library(raster)
library(tidyverse)
library(foreach)
library(doParallel)

corz <- detectCores()-1
path <- "/home/a/data/landsat/p42r31"
l_files <- list.files(path, full.names = T)

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
foreach(i = 1:length(l_files))%dopar%{
  ee<- extent(raster(l_files[1]))
  for (j in 1:length(l_files)){
    r<- raster(l_files[j])
    if(extent(r)@xmin > ee@xmin) ee@xmin <- extent(r)@xmin
    if(extent(r)@xmax < ee@xmax) ee@xmax <- extent(r)@xmax
    if(extent(r)@ymin > ee@ymin) ee@ymin <- extent(r)@ymin
    if(extent(r)@ymax < ee@ymax) ee@ymax <- extent(r)@ymax
  }
  rr <- raster::stack(l_files[i]) %>%
    crop(ee)
  writeRaster(rr, filename = l_files[i], overwrite =TRUE)
  gc()
  raster::removeTmpFiles()
  system(paste("echo",i))
}

