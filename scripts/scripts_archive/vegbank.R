
# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
# install.packages("aws.s3")
# library(aws.s3)

tmpd<- paste0("data/tmp")
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

source("/home/rstudio/wet_dry/scripts/functions.R")

# import data ------------------------------------------------------------------
# syntax for s3 is: aws s3 sync <s3 bucket location> <local location>

system("aws s3 sync s3://earthlab-amahood/data/vegbank /home/rstudio/wet_dry/data/vegbank")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl /home/rstudio/wet_dry/data/landfire_esp_rcl")

env <-  read_csv("data/vegbank/plot_env.csv") %>%
  dplyr::select(observation_id,
                date = obsstartdate_vb,
                latitude,
                longitude)

tfol <- read_csv("data/vegbank/plot_taxa.csv") %>%
  dplyr::select(observation_id, cover) %>%
  group_by(observation_id) %>%
  summarise(total_foliar_cover = sum(cover, na.rm=T))

plot_data <- read_csv("data/vegbank/plot_taxa.csv") %>%
  dplyr::select(observation_id, authorplantname_vb, cover) %>% # also there is stratum
  mutate(cover = as.numeric(cover)) %>%
  mutate(cover = replace(cover, is.na(cover)==T, 0),
         dup = duplicated(dplyr::select(., observation_id,authorplantname_vb))) %>%
  filter(dup == FALSE) %>% # a couple of unknowns with the same name in the same plot
  dplyr::select(-dup)%>%
  spread(authorplantname_vb, cover, fill=0) %>%
  dplyr::select(observation_id, brte=`Bromus tectorum L.`, starts_with("Artemisia")) %>%
  mutate(sage = rowSums(.[3:22])) %>%
  dplyr::select(sage, brte, observation_id) %>%
  left_join(tfol) %>%
  left_join(env) %>%
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")




ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") #ecoregions
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp") #landsat scenes
binary_clip <- raster("data/landfire_esp_rcl/clipped_binary.tif") #clipped (in arcmap) shrub binary

plot_data <- plot_data %>% st_transform(crs = crs(binary_clip, asText = TRUE)) %>%
  mutate(binary = raster::extract(binary_clip, plot_data)) %>%
  filter(binary >0)

# tweak data -------------------------------------------------------------------
great_basin <- subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain"))


great_basin <- st_transform(great_basin, st_crs(plot_data)) %>% st_union() # dissolve into one polygon
gb_plots <- st_intersection(plot_data,great_basin) %>%
  mutate(year = substr(as.character(date),1,4)) # making a year field

scenes <- st_transform(scenes,st_crs(gb_plots)) %>%
  filter(ROW<100) # getting rid of path/rows we don't want 
gb_plots <- st_intersection(gb_plots,scenes[,9:10]) # grabbing only the row and path
gb_plots$path_row <- as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 

landsat_s3 <- "s3://earthlab-amahood/data/landsat"
landsat_local <- "/home/rstudio/wet_dry/data/landsat"
dir.create("data/scrap", showWarnings = FALSE)
exdir <- "data/scrap/"

years <- unique(gb_plots$year) # getting the years plots were monitored - 2011-2015
path_row_combos <- unique(gb_plots$path_row) # this gives us the unique path row combinations

# this might have to be modified to fit the st_tranform syntax
gb_plots <- st_transform(gb_plots, crs=st_crs(plot_data))

gbplots_list <- list()
counter <- 1 # can't parallelize with this counter system
kounter <- 1
needs <- data.frame(y = NULL, pr=NULL)
n <- 1
for(i in 1:length(years)){ 
  
  system(paste0("aws s3 sync ",
                landsat_s3, "/landsat_",years[i], " ",
                landsat_local, "/", years[i], 
                " --only-show-errors"))
  
  for(j in 1:length(path_row_combos)){
    
    # subsetting the plots for the year and row/column combination
    print("subsetting")
    gbplots_subset <- gb_plots[gb_plots$path_row == path_row_combos[j]
                               & gb_plots$year == years[i],]
    print(paste(round(counter/125*100), "%")) #progress indicator
    
    if(nrow(gbplots_subset) > 0) {
      # listing all the files with the right year and path/row combo
      
      tar_files <- Sys.glob(paste0(landsat_local,"/", years[i], "/","LT05", 
                                   path_row_combos[j],
                                   "*.tar.gz")) 
      if(length(tar_files)>0){
        dates<- substr(tar_files, 55,58) %>%
          as.numeric()
        
        print("untarring")
        untar(tar_files[which.max(dates)], exdir = exdir)
       
        #listing only the tif files we've extracted (there's a bunch of metadata etc too, which you should familiarize yourself with)
        tif_files <- Sys.glob(paste0(exdir,"*.tif"))
        
        
        print("looping through the tifs")
        for(k in 1:length(tif_files)){
          # now we loop through each tif file and extract the values
          band <- raster::raster(tif_files[k]) # this just loads the raster
          x <- strsplit(names(band),"_")[[1]]
          bandname <- paste0(x[8:length(x)], collapse="_")
          gbplots_subset$band <- raster::extract(band, gbplots_subset) # and here we extract
          names(gbplots_subset)[length(names(gbplots_subset))] <- bandname
        }
        print("tifs looped")
        gbplots_subset <- st_transform(st_as_sf(gbplots_subset),crs = st_crs(plot_data))
        file.remove(Sys.glob(paste0(exdir,"*"))) # now we delete the tifs
        print("extracted")
        
        #renaming columns
            #adding the data to a list - converting back to a shapefile will be easy later, 
        #since they have lat and long as data frame columns
        gbplots_list[[kounter]] <- gbplots_subset
        
        if(kounter == 1){
          result <- gbplots_subset
        }else{
          result <- rbind(result, gbplots_subset)
        }
        
        
        kounter <- kounter + 1
      }else {print("nofile");needs[n,1] <- years[i];needs[n,2] <- path_row_combos[j];n<-n+1}
    }else{print("nopoints")}
    counter <- counter + 1 
    
    
  }
  system(paste0("rm -r ",landsat_local, "/", years[i] )) #deleting the unneeded landsat files
}


####NEW LANDFIRE DEM HERE ----------------------------------------------------------
system("aws s3 sync s3://earthlab-amahood/data/terrain_gb /home/rstudio/wet_dry/data/terrain")
# gb_dem <- raster("data/terrain/elevation.tif")

# terrain raster --------------------------------------------------------------------------
# ter_s3 <- 's3://earthlab-amahood/data/terrain_2'
# ter_local <- '/home/rstudio/wet_dry/data/terrain_2'
# 
# system(paste("aws s3 sync",
#              ter_s3,
#              ter_local))
# opts <- c('slope', 'aspect', 'TPI', 'TRI', 'roughness','flowdir')
# 
# cores <- length(opts)
# 
# registerDoParallel(cores)
# 
# foreach(i = opts) %dopar% {
#   filename <- paste0("data/terrain_2/", i,".tif")
#   if(!file.exists(filename)){
#     ter_rst <- terrain(gb_dem,
#                        opt = i,
#                        unit = 'degrees',
#                        neighbors = 8, 
#                        format = 'GTiff',
#                        filename = filename)
#     system(paste0("aws s3 cp ",
#                   filename,
#                   " s3://earthlab-amahood/",filename))
#   }
# }
# #### create terrain raster stack ####
# slope_gb <- raster("data/terrain_2/slope.tif")
# aspect_gb <- raster("data/terrain_2/aspect.tif")
# TPI_gb <- raster("data/terrain_2/TPI.tif")
# TRI_gb <- raster("data/terrain_2/TRI.tif")
# roughness_gb <- raster("data/terrain_2/roughness.tif")
# flowdir_gb <- raster("data/terrain_2/flowdir.tif")
# 
# terrain_files <- list.files(ter_local, full.names = T)
# terrain_gb <- stack(terrain_files)
# terrain_gb <- brick(terrain_gb)
# 
# writeRaster(terrain_gb, "home/rstudio/wet_dry/data/terrain_2/terrain_gb.tif", progress = "text")
# 
# system("aws s3 cp home/rstudio/wet_dry/data/terrain_2/terrain_gb.tif s3://earthlab-amahood/data/terrain_2/terrain_gb.tif")
# keep only sunny days ----------------------------------------------------------------------

df <- result[result$pixel_qa == 66,] %>%
  na.omit

# Create vegetation indices -------------------------------------------------------------------
df$ndvi <- get_ndvi(df$sr_band3,df$sr_band4)
df$evi <- get_evi(df$sr_band1, df$sr_band3, df$sr_band4)
df$savi <- get_savi(df$sr_band3,df$sr_band4)
df$sr <- get_sr(df$sr_band3,df$sr_band4)

df$greenness <- green5(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$brightness <- bright5(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)
df$wetness <- wet5(df$sr_band1,df$sr_band2,df$sr_band3,df$sr_band4,df$sr_band5,df$sr_band7)

# extracting elevation and topography -----------------------------------------------

df$elevation <- raster::extract(raster("data/terrain/elevation.tif"), df)

df$slope <- raster::extract(raster("data/terrain/slope.tif"), df)
df$aspect <- raster::extract(raster("data/terrain/aspect.tif"), df)
df$tpi <- raster::extract(raster("data/terrain/tpi.tif"), df)
df$tri <- raster::extract(raster("data/terrain/tri.tif"), df)
df$roughness <- raster::extract(raster("data/terrain/roughness.tif"), df)
df$flowdir <- raster::extract(raster("data/terrain/flowdir.tif"), df)

df$folded_aspect = abs(180 - abs(df$aspect - 225))

# writing the file and pushing to s3 -----------------------------------------------
st_write(df, "data/vegbank_plots_with_landsat.gpkg", delete_layer = TRUE)
system("aws s3 cp data/vegbank_plots_with_landsat.gpkg s3://earthlab-amahood/data/vegbank_plots_with_landsat.gpkg")


