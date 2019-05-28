library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)

local_scrap<- "/home/a/data/scrap/"
dir.create(local_scrap)
#
gpkg_file <- ""
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

baecv_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "

dd <-st_read(gpkg_file)%>%
 # dplyr::select() %>%
  st_transform(st_crs(baecv_crs))

# change yr_samp to whatever the year of sampling
y_max <- max(dd$yr_samp)



corz <- detectCores() -1

registerDoParallel(corz)
results <- list()
counter <- 1
foreach(yy = 1984:y_max)%dopar%{
  s3_file<- paste0("s3://earthlab-ls-fire/v1.1/BAECV_",yy,"_v1.1_20170908.tar.gz")
  local_file<-paste0(local_scrap, "BAECV_",yy,"_v1.1_20170908.tar.gz")
  target_file<-paste0("BAECV_bc_",yy,"_v1.1_20170908.tif")
  system(paste("aws s3 cp", s3_file, local_file))
  system(paste("tar xvzf", local_file, "-C", local_scrap, target_file))
  
  bc_file <- paste0(local_scrap, target_file)
  bc<- raster(bc_file)
  
  results[[counter]] <- mutate(dd, burned = raster::extract(bc,dd), # burned will be 0 or 1
              burn_year = yy) # burn year will be year
  
  system(paste0("rm ", local_file))
  system(paste0("rm ", bc_file))
  gc()
  raster::removeTmpFiles(h=0.25)
  counter <- counter+1
}
final <-do.call("rbind", results) %>%
  dplyr::filter(burned > 0) %>%
  mutate(Study_ID = as.factor(Study_ID)) %>% # change to plot ID
  group_by(Study_ID) %>%
  summarise(lyb = max(burn_year))

st_write(final, "lyb_forthoseplots.gpkg")
