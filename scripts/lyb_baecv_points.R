#Title: Last Year Burned Point Extraction 
#Authors: Adam Mahood and Dylan Murphy
#Last Modified: 7/10/19

#1. Set up
#Load Packages
library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
library(dplyr)

#Create Scrap Directory
local_scrap<- "data/scrap/"
dir.create("data")
dir.create(local_scrap)

#pull point data from s3 bucket
#blm aim points
system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat_feb19.gpkg data/plot_data/plots_with_landsat.gpkg")

#naip manual points
system("aws s3 sync s3://earthlab-amahood/data/naip_trainingdata data/plot_data/")

#2. Data Prep

#create path object for point data

# if using BLM-AIM data use this 
gpkg_file <- "data/plot_data/plots_with_landsat.gpkg"

#if using NAIP manual points use this 
gpkg_file <- "data/plot_data/humboldt_nv013_spb_finalpoints.shp"

#grab crs of baecv rasters
baecv_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "

#### BLM - AIM data prep ####
#load point data, remove already extracted band values, reproject to match baecv raster crs, and add a year variable which isnt a factor
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

#### NAIP-manual training data data prep ####
dd <- st_read(gpkg_file) %>% mutate(plot_year = 2010,
                                    OBJECTID = rownames(dd))

#Grab most recent year in point dataset
y_max <- max(dd$plot_year)


#parallel prep
corz <- detectCores() /2
registerDoParallel(corz)


#### attaching last year burned (lyb) to NAIP plots ####
#create empty list to store results
results <- list()
#iterator
yy = 1984:y_max
#loop over each year's baecv raster and extract two variables: burned (binary) and burnyear


results <- foreach(i = 1:length(yy),
        .packages = 'raster') %dopar% {
  
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
  
  
  ddd <- dd %>% mutate(burned = raster::extract(bc,dd), # burned will be 0 or 1
              burn_year = yy[i]) # burn year will be year
  
  system(paste("echo", "burns extracted to training points", yy[i]))
  
  system(paste0("rm ", local_file))
  system(paste0("rm ", bc_file))
  gc()
  raster::removeTmpFiles(h=0.25)
  
  ddd
  #counter <- counter+1
}



#gather last year burned results and create new dataframe from them (grouped by plot ID)
final <-do.call("rbind", results) # change to plot ID
  
final <- final %>% dplyr::filter(burned > 0,
                burn_year < plot_year) %>% 
  group_by(OBJECTID) %>%
  summarise(lyb = max(burn_year)) 

final1 <- st_drop_geometry(final)
dd1 <- st_drop_geometry(dd)

final <- merge(dd1, final1, by = 'OBJECTID', all.x = T)

lyb_filename <- "lyb_gb_naip_humboldt_sbp_plots.gpkg"
#save and upload results to s3
st_write(final, lyb_filename)
system("aws s3 cp lyb_gb_naip_humboldt_sbp_plots.gpkg s3://earthlab-amahood/data/naip_trainingdata/lyb_gb_naip_humboldt_sbp_points.gpkg")

#### attaching lyb to BLM AIM plots ####

#FOR BLM DATA get original blm plot data and remove extracted band values
gbd <- st_read(gpkg_file) %>%
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
  st_transform(st_crs(baecv_crs))

#FOR NAIP MANUALLY CREATED DATA
gbd <- st_read(gpkg_file) %>% st_transform(st_crs(baecv_crs)) %>% mutate(OBJECTID = rownames(gbd))

#make gbd into a data frame without spatial info
gbd1 <- gbd %>% st_drop_geometry()

#make last year burned into a data frame without spatial info
lyb <- st_read(lyb_filename)
lyb1 <- lyb %>% st_drop_geometry()

#merge lyb info to gbd data using object id to match plots
gbd_lyb <- merge(gbd1, lyb1, by = "OBJECTID", all = T)

#reattach geometry to point data
gbd_lyb <- st_set_geometry(gbd_lyb, st_geometry(gbd))

#save and upload to s3
st_write(gbd_lyb, "data/naip_nv013_humboldt_points_w_lyb_July10.gpkg")
system("aws s3 cp data/naip_nv013_humboldt_points_w_lyb_July10.gpkg s3://earthlab-amahood/data/naip_trainingdata/naip_nv013_humboldt_points_w_lyb_July10.gpkg")




