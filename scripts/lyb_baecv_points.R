library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
library(dplyr)

local_scrap<- "data/scrap/"
dir.create("data")
dir.create(local_scrap)

system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")

gpkg_file <- "data/plot_data/plots_with_landsat.gpkg"
#latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

baecv_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "

dd <-st_read(gpkg_file) %>%
  dplyr::select(-sr_band1,
                -mean_sr_band1,
                -sr_band2,
                -mean_sr_band2,
                -sr_band3,
                -mean_sr_band3,
                -sr_band4,
                -mean_sr_band4,
                -sr_band5,
                -mean_sr_band5,
                -sr_band7,
                -mean_sr_band7,
                -sr_cloud_qa,
                -ndvi, - evi, -savi, -sr, 
                -greenness, -wetness, -brightness) %>%
    dplyr::mutate(plot_year = as.numeric(as.character(dd$year))) %>%
  st_transform(st_crs(baecv_crs))

# change yr_samp to whatever the year of sampling
y_max <- max(dd$plot_year)



corz <- detectCores() /2

registerDoParallel(corz)
results <- list()
counter <- 1
for(i in 1:32) {
  yy = 1984:y_max
  s3_file<- paste0("s3://earthlab-ls-fire/v1.1/BAECV_",yy[i],"_v1.1_20170908.tar.gz")
  local_file<-paste0(local_scrap, "BAECV_",yy[i],"_v1.1_20170908.tar.gz")
  target_file<-paste0("BAECV_bc_",yy[i],"_v1.1_20170908.tif")
  system(paste("aws s3 cp", s3_file, local_file))
  system(paste("echo", "baecv downloaded", yy[i]))
  system(paste("tar xvzf", local_file, "-C", local_scrap, target_file))
  system(paste("echo", "baecv untarred", yy[i]))
  system(paste0("rm ", local_file))
  
  bc_file <- paste0(local_scrap, target_file)
  bc<- raster(bc_file)
  system(paste("echo", "baecv raster created", yy[i]))
  
  
  results[[i]] <- dd %>% mutate(burned = raster::extract(bc,dd), # burned will be 0 or 1
              burn_year = yy[i]) # burn year will be year
  
  system(paste("echo", "burns extracted to training points", yy[i]))
  
  system(paste0("rm ", local_file))
  system(paste0("rm ", bc_file))
  gc()
  raster::removeTmpFiles(h=0.25)
  #counter <- counter+1
}

final <-do.call("rbind", results) %>%
  dplyr::filter(burned > 0,
                burn_year < plot_year) %>%
  mutate(plot_ID = as.factor(OBJECTID)) %>% # change to plot ID
  group_by(plot_ID) %>%
  summarise(lyb = max(burn_year))

st_write(final, "lyb_gb_blm_plots.gpkg")
system("aws s3 cp lyb_gb_blm_plots.gpkg s3://earthlab-amahood/data")

final

