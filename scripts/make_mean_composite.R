# script to input analysis ready landsat data, make mean composite
library(raster)
library(tidyverse)
library(foreach)
library(doParallel)
ls_ard_path <- "/home/a/data/landsat_ard"
scrap_path <- "scrap"
dir.create(scrap_path)
dir.create("data/mean_composites")
tar_files <- list.files(ls_ard_path, pattern = ".tar", full.names = TRUE)


stks <- list()
for(i in 1:length(tar_files)){
  scrap_path_i <- file.path(scrap_path, i)
  dir.create(scrap_path_i)
  untar(tar_files[i], exdir = path.expand(scrap_path_i))
  bands <- list.files(scrap_path_i, pattern = "SRB", full.names = TRUE)
  qa <- list.files(scrap_path_i, pattern = "PIXELQA", full.names = TRUE) %>%
    raster()
  qa[qa != 66] <- NA
  stk <- raster::stack(bands) 
  stks[[i]] <- stk * qa
  
  unlink(scrap_path_i,recursive = TRUE)
  print(i)
}

for(b in 1:6){
  
  if(b == 6) {bandn <- 7}else{bandn <- b}
  mc<- list()
    for(i in 1:length(tar_files)){
      mc[[i]] <- stks[[i]][[b]]
    }
  mc1 <- raster::stack(mc)
  calcd <-calc(mc1,fun=mean, na.rm = TRUE)
  
  writeRaster(calcd, filename = paste0("data/mean_composites/mc_band_",bandn, ".tif"))
  rm(mc)
  rm(mc1)
  raster::removeTmpFiles(h=0.001)
  gc()
}
