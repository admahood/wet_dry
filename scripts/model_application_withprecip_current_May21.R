# Title: Random Forest Model Application - GB Land Cover Classification Using Precip Anomaly
# Author(s): Dylan Murphy
# Date last modified: 5/23/19

#### 1.1: Setup - Load Packages/Source Scripts
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel", "gdalUtils")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("scripts/functions.R")

#### 1.2: Setup - Create Directories
dir.create("data/results")
tmpd<- paste0("data/tmp") # telling raster where to put its temp files so we can easily delete them
dir.create(tmpd)
rasterOptions(tmpdir=tmpd)

#### 1.3: Setup - Pull Data from S3
#landsat paths
s3_ls_path <- "s3://earthlab-amahood/data/ls5_mucc"
local_ls_path <- "data/ls5_mucc"
#terrain paths 
s3_terrain <- "s3://earthlab-amahood/data/terrain_2"
local_terrain <- "data/terrain_reproj_full"
#precip anomaly raster paths (cropped to NAIP scene already)
s3_precip <- "s3://earthlab-amahood/data/PRISM_precip_annual/naip_trimmed_annual_precip_anomaly"
local_precip <- "data/prism/naip_trimmed_annual_precip_anomaly"

#s3 syncs 
system(paste0("aws s3 sync ", s3_ls_path, " ", local_ls_path))
system(paste0("aws s3 sync ", s3_terrain, " ", local_terrain))
system(paste0("aws s3 sync ", s3_precip, " ", local_precip))

#s3 syncs cont. (masks)
system("aws s3 sync s3://earthlab-amahood/data/landfire_esp_rcl/ data/esp_binary")
system("aws s3 sync s3://earthlab-amahood/data/landfire_urban_ag_water_mask/ data/urban_ag_mask")

#s3 syncs cont. (naip)
system("aws s3 sync s3://earthlab-amahood/data/naip data/naip")

#grab filenames for ls5 stacks 
scene <- list.files("data/ls5_mucc")
scene_full <- list.files("data/ls5_mucc", full.names = T)

#### 1.4: Setup - Parallelization
cores <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoParallel(cores)

#### 2. Model Application Loop (Parallized)

#get landsat crs
landsat <- stack(scene_full[1])
crs <- crs(landsat)

#create naip scene raster object for cropping 
naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") %>% projectRaster(crs = crs, res = 30) #wmuc
#naip <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") %>% projectRaster(crs = crs, res = 30) #frank
#naip <- raster("data/naip/m_4111823_sw_11_1_20100628.tif") %>% projectRaster(crs = crs, res = 30) #kings
#parallelized model application loop
foreach(i = scene_full, 
        .packages = 'raster') %dopar% {         
         
          #grab file name without directories for filename creation later 
          file = substr(i, 15, 36) 
          
          #grab year for particular loop iteration
          year = substr(i, 19, 22)
          
          #grab start time for progress check 
          t0 <- Sys.time()
          
          #crop landsat data to naip scene
          ls5 <- stack(i) %>% crop(naip)
          
          #crop terrain data to naip scene
          ter <- stack(list.files(local_terrain, full.names =T)) %>% crop(naip) %>% resample(ls5) 
          
          #get proper names for landsat bands - important for use in veg indice/tassel cap functions later
          names(ls5)<- c("sr_band1", "sr_band2","sr_band3", "sr_band4","sr_band5", "sr_band7")
          
          #progress check
          system(paste("echo", "stack created and cropped", i))
          
          #grab precip anomaly for a particular year - change "frank"/"wmuc"/"kings" in both folder and filename depending on which you want to use
          precip <- raster(paste0("data/prism/naip_trimmed_annual_precip_anomaly/wmuc/precip_anomaly_trimmed_wmuc", year, ".tif")) %>% resample(ls5)
          
          # create additional index variables - make sure all the names of this stack match the names that go into the model 
          ls5$wetness <- wet5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$brightness <- bright5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$greenness <- green5(ls5$sr_band1,ls5$sr_band2,ls5$sr_band3,ls5$sr_band4,ls5$sr_band5,ls5$sr_band7)
          ls5$ndvi <- get_ndvi(ls5$sr_band3, ls5$sr_band4)
          ls5$savi <- get_savi(ls5$sr_band3, ls5$sr_band4)
          ls5$sr <- get_sr(ls5$sr_band3, ls5$sr_band4)
          ls5$evi <- get_evi(ls5$sr_band1, ls5$sr_band3, ls5$sr_band4)
          ls5$ndsvi <- get_ndsvi(ls5$sr_band3, ls5$sr_band5)
          ls5$satvi <- get_satvi(ls5$sr_band3, ls5$sr_band5, ls5$sr_band7)
          system(paste("echo", "veg indices and tassel cap created", i))
          
          #create stack of ls5 data, terrain data, and precip anomaly
          ls5 <- stack(ls5, 
                       ter, 
                       precip)
          
          #create esp mask and match projection/extent
          esp_mask <- raster("data/esp_binary/clipped_binary.tif")
          esp_mask <- projectRaster(esp_mask, ls5, res = 30)
          system(paste("echo", "esp mask reprojected", i))
          
          #create urban_ag mask and match projection/extent
          urb_mask <- raster("data/urban_ag_mask/lf_msk_rclss1.tif")
          urb_mask <- projectRaster(urb_mask, ls5, res = 30)
          system(paste("echo", "esp mask reprojected", i))
          
          #masking (esp)
          ls5 <- mask(ls5, esp_mask, maskvalue = 0)
          system(paste("echo", "esp masking done", i))
          
          #masking (urb_ag_Water)
          ls5 <- mask(ls5, urb_mask, maskvalue = 1)
          system(paste("echo", "urb ag masking done", i))
          
          # names to match exactly with training data that goes into model. 
          # The order matters for these
          names(ls5) <- c("sr_band1", "sr_band2", "sr_band3", "sr_band4", 
                          "sr_band5", "sr_band7", "wetness", "brightness", 
                          "greenness",  "ndvi", "savi", "sr", "evi",
                          "ndsvi", "satvi", "flowdir", "folded_aspect", #"elevation",
                          "roughness", "slope", "tpi", "tri", 
                          "precip_anomaly")
          
          #progress check for stack creation
          system(paste("echo", "stack created"))
          print(Sys.time()-t0)
          
          #for saving memory
          gc() 
          
          #make filename - change "frank"/"wmuc"/"kings" depending on naip scene used for extent
          filenamet <- paste0("data/results/", "tstrained_three_class_model_results_w_precip", "_wmuc_", year, "_Jun13", ".tif") 
          system(paste("echo", "filename created", i))
          
          #apply the RF model to raster stack and create "ls5_classed", an annual predicted sage/cheat raster!
          ls5_classed <- raster::predict(ls5, model, inf.rm = T, na.rm = T,) #apply model of choice from list to stack and make predictions
          
          #progress check
          system(paste("echo", "model applied"))
          print(Sys.time()-t0) 
          
          #save resulting land cover rasters and upload to s3
          writeRaster(ls5_classed, filename = filenamet, format = "GTiff", overwrite = T) 
          system(paste("echo", "file saved to disk"))
          system(paste0("aws s3 cp ", filenamet, " s3://earthlab-amahood/data/summer19_model_results/Jun13_tstrained_3class_modelrun_w_precip2/wmuc/", substr(filenamet, 14, 150)))
          system(paste("echo", "aws sync done"))
          
        }

#### Extracting pixel counts to data table and quickly viewing through plots ####

#list results raster files
dir.create("data/results")
#change path below to desired model results folder on s3
system("aws s3 sync s3://earthlab-amahood/data/summer19_model_results/Jun13_tstrained_3class_modelrun_w_precip2/ data/results")

#change "kings"/"frank"/"wmuc" to select results for a specific naip scene 
all_years_files <- list.files("data/results/wmuc", pattern = "\\.tif$", full.names = T)
all_years_stack <- stack(all_years_files)

results_list <- list()


for(i in 1:length(all_years_files)) {
  results  <- raster(all_years_files[i])
  #results <- raster::extend(results, total_na)
  #results <- crop(results, total_na)
  results_list[i] <- results
}
#grab each years predicted values and store in vector form
df2 <- rbind( 
  r1984 = as.vector(table(values(results_list[[1]])))
  , r1985 = as.vector(table(values(results_list[[2]])))
  , r1986 = as.vector(table(values(results_list[[3]])))
  , r1987 = as.vector(table(values(results_list[[4]])))
  , r1988 = as.vector(table(values(results_list[[5]])))
  , r1989 = as.vector(table(values(results_list[[6]])))
  , r1990 = as.vector(table(values(results_list[[7]])))
  , r1991 = as.vector(table(values(results_list[[8]])))
  , r1992 = as.vector(table(values(results_list[[9]])))
  , r1993 = as.vector(table(values(results_list[[10]])))
  , r1994 = as.vector(table(values(results_list[[11]])))
  , r1995 = as.vector(table(values(results_list[[12]])))
  , r1996 = as.vector(table(values(results_list[[13]])))
  , r1997 = as.vector(table(values(results_list[[14]])))
  , r1998 = as.vector(table(values(results_list[[15]])))
  , r1999 = as.vector(table(values(results_list[[16]])))
  , r2000 = as.vector(table(values(results_list[[17]])))
  , r2001 = as.vector(table(values(results_list[[18]])))
  , r2002 = as.vector(table(values(results_list[[19]])))
  , r2003 = as.vector(table(values(results_list[[20]])))
  , r2004 = as.vector(table(values(results_list[[21]])))
  , r2005 = as.vector(table(values(results_list[[22]])))
  , r2006 = as.vector(table(values(results_list[[23]])))
  , r2007 = as.vector(table(values(results_list[[24]])))
  , r2008 = as.vector(table(values(results_list[[25]])))
  , r2009 = as.vector(table(values(results_list[[26]])))
  , r2010 = as.vector(table(values(results_list[[27]])))
  , r2011 = as.vector(table(values(results_list[[28]])))
)
#turn vectors into data frame
df2 <- as_data_frame(df2)

#name the class total columns
names(df2) <- c("grass", "shrub", "mixed")

#create additional class total variables (e.g. percentages of total study area, etc)

df2 <- df2 %>% mutate(year = c(1984:2011),
                      total_pixels = as.numeric(grass + shrub + mixed),
                      percent_grass = as.numeric((grass / total_pixels) * 100),
                      percent_shrub = as.numeric((shrub / total_pixels) * 100),
                      percent_mixed = as.numeric((mixed/total_pixels) * 100), 
                      shrubvgrass = as.numeric(shrub/grass))

#plot class totals over time using ggplot - switch "grass" to "shrub" or vice versa to look at each class

ggplot(data=df2) + 
  aes(x = df2$year) + 
  geom_point(aes(y=df2$percent_mixed), color = 'darkgreen') + 
  geom_smooth(aes(y=df2$percent_mixed), color = 'darkgreen', method = "lm") +
  xlab("year") +
  ylab("Sagebrush total pixels")

#### Animating Results for quick viewing - not working yet ####
anim_libs <- c("gganimate","gifski")
lapply(anim_libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(anim_libs, library, character.only = TRUE, verbose = FALSE)

lcc_rasters <- list.files("data/results/wmuc", pattern = "\\.tif$", full.names = T)

lcc_stack <- stack(lcc_rasters)

years=1984:2011
ts_df=list()

for (i in 1:length(years)) {
  rrr<- raster(lcc_rasters[i]) 
  rr <- as.data.frame(rrr, xy = TRUE)
  names(rr) <- c("x","y", "Prediction")
  nn <- lcc_rasters[i]
  rr$year=as.numeric(substr(nn, 69, 72))
  ts_df[[i]]<-rr
}

ts_df<-do.call("rbind",ts_df)

anim<-ggplot(ts_df, aes(x=x,y=y,fill=as.numeric(Prediction)))+
  geom_raster() +
  theme_void() +
  scale_fill_viridis_c() +
  coord_fixed()+
  labs(title = 'Year: {frame_time}') +
  transition_time(year)

aa<-gganimate::animate(anim, fps=2, nframes = length(years))



