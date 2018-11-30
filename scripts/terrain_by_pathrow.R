# setup --------------------------------------------------------------------------------------
library(sf)
library(raster)
# syncing files ------------------------------------------------------------------------------
system("aws s3 sync s3://earthlab-amahood/data/terrain_gb data/terrain")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")

ter_path <- "data/terrain" 
ter_files <- list.files(ter_path)[2:8] #don't need aspect

dir.create("scrap")

ter_crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

great_basin <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") %>%
  subset(NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain")) %>%
  st_transform(st_crs(ter_crs))%>%
  st_union() # dissolve into one polygon

# get scenes -------------------------------------------------------------------
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp")  %>%
  st_transform(st_crs(ter_crs))

scenes_gb <- scenes %>%
  dplyr::filter(ROW<100) %>% # getting rid of path/rows we don't want 
  st_intersection(great_basin) 

path_row <- as.character(paste0(scenes_gb$PATH,scenes_gb$ROW)) 

for(i in 1:length(path_row)){
  P <- substr(path_row[i],1,2)
  R <- substr(path_row[i],3,4)
  scene <- dplyr::filter(scenes, PATH == P, ROW == R) %>%
    st_buffer(dist = 20000)
  
  for(j in 1:length(ter_files)){
    dir.create("scrap")
    p <- "scrap/"
    f <- paste0("p",P,"_r",R,"_",ter_files[j])
    r <- raster(file.path(ter_path,ter_files[j])) %>%
      crop(scene, filename = paste0(p,f))
    
    system(paste0("aws s3 cp ",
                  p,f,
                  " s3://earthlab-amahood/data/terrain_by_pathrow/",
                  P,R,"/",f))
    unlink(paste0(p,f))
    
  }
}