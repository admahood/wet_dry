#Title: Creating Mean Composite Images from Landsat Analysis Ready Data (ARD)
#Author(s): Adam Mahood, Dylan Murphy
#Date Created: 7/18/19
#Date last modified: 7/25/19


#### 1. Set Up ####

#Load Packages
libs <- c("raster", "tidyverse", "foreach", "doParallel", "stringr")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#Create path objects

ls_ard_path_local <- "data/landsat_ard"
ls_ard_path_s3 <- "s3://earthlab-amahood/wet_dry/input_raster_data/landsat_ard_ndvi_informed/"

#create data directory 
dir.create("data")

#create scrap directory
scrap_path <- "scrap"
dir.create(scrap_path)

#create directory for resulting mean composite images
dir.create("data/mean_composites")

#pull data from s3
system(paste0("aws s3 sync ", ls_ard_path_s3, " ", ls_ard_path_local))

#### 2. Untarring Scenes, stacking .tif bands, and creating mean composite imagery ####

#list .tar files in folder 
tar_files <- list.files(ls_ard_path_local, pattern = ".tar", full.names = TRUE)

# SELECTING ARD DATA FROM LS5, LS7, OR BOTH:

  #LS5:
    tar_files <- tar_files %>% str_subset(pattern = fixed("LT05"))
    sensor_platform <- "ls5"
    
  #LS7: 
    tar_files <- tar_files %>% str_subset(pattern = fixed("LE07"))
    sensor_platform <- "ls7"
    
  #BOTH: NO ADDITIONAL SUBSETTING NEEDED
    sensor_platform <- "ls5_and_ls7"
#create empty vectors and lists to store values & rasters
years <- c()
year_season <- c()
stks <- list()

#list already completed mean composite rasters 
mc_files_done <- system(paste0("aws s3 ls s3://earthlab-amahood/wet_dry/derived_raster_data/mean_composites_ndvi/", sensor_platform, "/"), intern = T)

#grab year/season of already completed mc rasters to compare against when looping over new files
year_season_done <- c()
for(i in 1:length(mc_files_done)) {
  year_season_done[i] <- substr(mc_files_done[i], 35, 44)
}

if(nchar(year_season_done[1]) == 0) {year_season_done <- year_season_done[-1]}

#if no files are done, make year_season_done = NA instead of a blank character vector
if(nchar(year_season_done[1]) == 0) {year_season_done <- NA}


#loop over tar file list, untar, stack bands together, store in a list (stks)
for(i in 1:length(tar_files)){
  
  #grab year for tar file
  years[i] <- substr(tar_files[i], 33, 36) 
  
  #grab season for tar file (may/june = spring, july/aug/later = summer)
  if(substr(tar_files[i], 38, 38) == 1 | substr(tar_files[i], 38, 38) == 0) {
    season <- "summer"
  } else if(substr(tar_files[i], 38, 38) < 7) {
    season <- "spring"
  } else {
    season <- "summer"
  }
  
  #combine year and season to identify image and store in vector
  year_season_single <- paste0(years[i], season)
  
  #create logical vector to check for matches in the list of already completed year/seasons
  already_done_check <- grepl(year_season_single, year_season_done)
  
  #condense logical vector; if any completed year/seasons match, then already_done_check is TRUE
  already_done_check <- any(already_done_check)
  
  #if the file is already done, print message telling us so and move on to next iteration
  if(already_done_check == TRUE) {
    year_season[i] <- NA
    print(paste0(year_season_single, " mean composite already created"))
    
    #if the mean composite is not already created, then create it 
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
year_season <- year_season[1:length(stks)]

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
  mc <- list()
  
  #subset raster stacks (stks) for particular year/season combo
  ys_stks <- stks[names(stks) == ys]
  
  #remove NULL values introduced into list by coercion in the first untarring and stacking loop 
  ys_stks <- Filter(Negate(is.null), ys_stks)
    
  #loop over subset of landsat stacks and grab all rasters for a particular band
    for(i in 1:length(ys_stks)){
      mc[[i]] <- ys_stks[[i]][[b]]
    }
  
  #stack specified band rasters into one raster stack 
  mc1 <- raster::stack(mc)
  
  #take mean pixel value across all rasters for specified band
  calcd <- mean(mc1, na.rm = T)
  
  #set name for mean composite based on year/season and band
  names(calcd) <- paste0(year_season_unique[y], "_sr_band", bandn, "_mc")
  
  #store mean composite raster for a single band in a list for later stacking
  calcd_list[[b]] <- calcd
 
}
  #stack mean composite rasters for a particular year/season into one stack w/ all bands  
  calcd_stk <- stack(calcd_list)
  
  #create filename objects (with & without full path) for saving/s3 upload
  filename = paste0("data/mean_composites/", "mc_",year_season_unique[y], "_ndvi_ts_", sensor_platform, ".tif")
  filename_short = paste0("mc_",year_season_unique[y], "_ndvi_ts_", sensor_platform, ".tif")
  
  #save mean composite stack to disk
  writeRaster(calcd_stk, filename = filename)
  
  #upload to s3 bucket
  system(paste0("aws s3 cp ", filename, " ", "s3://earthlab-amahood/wet_dry/derived_raster_data/", "mean_composites_ndvi/", sensor_platform, "/", filename_short))
  
}


