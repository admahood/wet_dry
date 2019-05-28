library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
csv_file <- "/home/a/Desktop/baecv_no_ll.csv"
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
baecv_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "
corz <- detectCores() -1


  
gb_fires <- st_read("/home/a/data/fire/wf1870-2015/Wildfires_1870_2015_Great_Basin.shp")

dd <-read_csv(csv_file)%>%
  mutate(duped = duplicated(lat)) %>%
  filter(duped==F)%>%
  dplyr::select(-duped,-topdepth_cm, -bottomdepth_cm, -BD_estimated,-veg,-thick,
                -Article_ID, -pool,-pool_value) %>%
  st_as_sf(coords=c("long","lat"), crs = latlong) %>%
  st_transform(st_crs(baecv_crs))

dd_gb<- dd%>%
  st_transform(st_crs(gb_fires))%>%
  st_intersection(gb_fires) %>%
  filter(Fire_Year < yr_samp)

y_max <- max(dd$yr_samp)

local_scrap<- "/home/a/data/scrap/"
dir.create(local_scrap)
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
  dd <-mutate(dd, burned = raster::extract(bc,dd),
              burn_year = yy)
  results[[counter]] <- dd
  
  system(paste0("rm ", local_file))
  system(paste0("rm ", bc_file))
  gc()
  raster::removeTmpFiles(h=0.25)
  counter <- counter+1
}
final <-do.call("rbind", results) %>%
  dplyr::filter(burned > 0) %>%
  mutate(Study_ID = as.factor(Study_ID)) %>%
  group_by(Study_ID) %>%
  summarise(lyb = max(burn_year))

st_write(final, "lyb_forthoseplots.gpkg")
