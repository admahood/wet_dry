#Title: Creating Mean Composite Images from Landsat Analysis Ready Data (ARD)
#Author(s): Adam Mahood, Dylan Murphy
#Date Created: 7/18/19
#Date last modified: 7/22/19


#### 1. Set Up ####

#Load Packages
libs <- c("raster", "tidyverse", "foreach", "doParallel", "stringr")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Create path objects

ls_ard_path_local <- "data/landsat_ard"
ls_ard_path_s3 <- "s3://earthlab-amahood/wet_dry/input_raster_data/landsat_ard/"

#create data directory 
dir.create("data")

#create scrap directory
scrap_path <- "scrap"
dir.create(scrap_path)

#create directory for resulting mean composite images
dir.create("data/mean_composites")

#pull data from s3
system(paste0("aws s3 sync ", ls_ard_path_s3, " ", ls_ard_path_local))

a#### 2. Untarring Scenes, stacking .tif bands, and creating mean composite imagery ####

#list .tar files in folder 
tar_files <- list.files(ls_ard_path_local, pattern = ".tar", full.names = TRUE)

#create empty vectors and lists to store values & rasters
years <- c()
year_season <- c()
stks <- list()

mc_files_done <- system("aws s3 ls s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites/", intern = T)

year_season_done <- c()
for(i in 1:length(mc_files_done)) {
  year_season_done[i] <- substr(mc_files_done[i], 35, 44)
}



#loop over tar file list, untar, stack bands together, store in a list (stks)
for(i in 1:length(tar_files)){
  
  #grab year for tar file
  years[i] <- substr(tar_files[i], 33, 36) 
  
  #grab season for tar file (may/june = spring, july/aug = summer)
  if(substr(tar_files[i], 38, 38) < 7) {
    season <- "spring"
  } else {
    season <- "summer"
  }
  
  #combine year and season to identify image and store in vector
  year_season_single <- paste0(years[i], season)
  
  already_done_check <- grepl(year_season_single, year_season_done)
  
  already_done_check <- any(already_done_check)
  
  if(already_done_check == TRUE) {
    year_season[i] <- NA
    print(paste0(year_season_single, " mean composite already created"))
  } else {
  year_season[i] <- year_season_single
    
  #create path to store untarred files
  scrap_path_i <- file.path(scrap_path, i)
  dir.create(scrap_path_i)
  
  #untar landsat imagery
  untar(tar_files[i], exdir = path.expand(scrap_path_i))
  
  #list paths to bands in .tif format
  bands <- list.files(scrap_path_i, pattern = "SRB", full.names = TRUE)
  
  #list path to quality assessment raster in .tif format and create raster object
  qa <- list.files(scrap_path_i, pattern = "PIXELQA", full.names = TRUE) %>%
    raster()
  
  #isolate cloudy pixels in qa raster (? confirm this step with adam)
  qa[qa != 66] <- NA
  
  #stack bands into one raster object
  stk <- raster::stack(bands) 
  
  #remove clouds from stack and store new raster stack in list
  stks[[i]] <- stk * qa
  
  #name landsat raster based on year/season and original name (for later when we need to seperate by tile)
  names(stks[[i]]) <- paste0(year_season[i], "_", names(stk))
  
  #delete tifs to save disk space
  unlink(scrap_path_i,recursive = TRUE)
  
  #progress check 
  print(i)
  }
}
  

#change names of raster stacks in list to simpler format (comment this out when we start to work with multiple tiles)
names(stks) <- year_season



#grab unique years/seasons present in raster stacks
year_season_unique <- unique(year_season[!is.na(year_season)])

#create empty list object to store mean composites for each band
calcd_list <- list()

#loop over each year/season combo and each band, create mean composite 
#from multiple images for each band, stack into one raster, save and upload to s3 bucket
for(y in 1:length(year_season_unique)) {
for(b in 1:6){
  
  #grab band number (excluding band 6)
  if(b == 6) {bandn <- 7}else{bandn <- b}
  
  #grab year/season combo for subsetting landsat files
  ys <- year_season_unique[y]
  
  #create empty list to store rasters for creation of mean composite image
  mc<- list()
  
  #subset raster stacks (stks) for particular year/season combo
  ys_stks <- stks[names(stks) == ys]
  ys_stks <- Filter(Negate(is.null), ys_stks)
    
  #loop over subset of landsat stacks and grab all rasters for a particular band
    for(i in 1:length(ys_stks)){
      mc[[i]] <- ys_stks[[i]][[b]]
    }
  
  #stack specified band rasters into one raster stack 
  mc1 <- raster::stack(mc)
  
  #take mean pixel value across all rasters for specified band
  calcd <-calc(mc1,fun=mean, na.rm = TRUE)
  
  #set name for mean composite based on year/season and band
  names(calcd) <- paste0(year_season_unique[y], "_sr_band", bandn, "_mc")
  
  #store mean composite raster for a single band in a list for later stacking
  calcd_list[[b]] <- calcd
  
  #writeRaster(calcd, filename = paste0("data/mean_composites/", "mc_band_",bandn, ".tif"))
  #rm(mc)
  #rm(mc1)
 
}
  #stack mean composite rasters for a particular year/season into one stack w/ all bands  
  calcd_stk <- stack(calcd_list)
  
  #create filename objects (with & without full path) for saving/s3 upload
  filename = paste0("data/mean_composites/", "mc_",year_season_unique[y], ".tif")
  filename_short = paste0("mc_",year_season_unique[y], ".tif")
  
  #save mean composite stack to disk
  writeRaster(calcd_stk, filename = filename)
  
  #upload to s3 bucket
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/wet_dry/derived_raster_data", "mean_composites/", filename_short))
  
}


