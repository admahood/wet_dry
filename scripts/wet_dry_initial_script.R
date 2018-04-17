################################### script for the wet_dry project ###################################
# Authors: Adam Mahood and Dylan Murphy
# Began: Sept 2017
# Last Modified:
#
# Question: does precipitation affect the probability of fire differently in sagebrush shrublands 
# and cheatgrass-dominated annual grasslands? -- how does veg structure affect disturbance probability 
# -- i.e. resilience
#
# Hypothesis: Annual grasslands will maintain a relatively high burn probability across precipitation
# levels, whereas shrublands will have significantly lower probabilities in dry years than wet years.
#
# Basic idea for the workflow: -- steps 1 & 2 possibly feasable by end of semester
# 1. use BLM plot data to classify landsat pixels as either shrubland or grassland
#     1a. clip to great basin
#     a. download landsat images for each year that blm plots were monitored somewhere around peak green
# (joe analytics hub guy is downloading landsat)
#     b. extract landsat band values for each plot location and add to the data frame
#     c. use machine learning/random forests/soemthing better? (account for autocorrelation?) to classify
#        landsat pixels
# 2. then classify all (or some number of random points) landsat pixels for the study area for each year (1984-2014)
#     a. mask ag roads water etc    clouds
#     i. identify points in time where pixels switched from shrub to grassland?
# 3. use fire data - mtbs, baecv - to figure out burn probabilities (need to figure out exactly what this means)
#         i. some kind of thing where there is a timeline of sorts for every plot?
#         ii. number of fires before and after transition points?
# 4. extract climate data to each point for each year
#     a. antecedent precipitation 1 and 2 winters prior
#
# 5. time series -- convergent cross mapping
#
#
# Data structure: long form. each line will have a point, year, veg state
#
#
# visualization idea: animation illustrating the difference

# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos")
lapply(libs, library, character.only = TRUE, verbose = FALSE)


# import data ------------------------------------------------------------------
system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM data/BLM_AIM")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions data/ecoregions")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc data/WRS2_paths/wrs2_asc_desc")


plot_data = st_read("data/BLM_AIM/BLM_AIM_20161025.shp") #BLM plots
ecoregions = st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") #ecoregions
scenes = st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp") #landsat scenes

# tweak data -------------------------------------------------------------------
great_basin = subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                  "Central Basin and Range", 
                                                  "Snake River Plain"))
great_basin = st_transform(great_basin, st_crs(plot_data)) # matching projections
great_basin = st_union(great_basin) # dissolve into one polygon
gb_plots = st_intersection(plot_data,great_basin) # clipping to great basin
gb_plots = gb_plots[!is.na(gb_plots$DateVisite),] # removing rows where the plot was not read
gb_plots$year = substr(as.character(gb_plots$DateVisite),1,4) # making a year field


# functions --------------------------------------------------------------------

brightness7 <- function(df) {
  x = (df$sr_band1 *0.3561) +
    (df$sr_band2 * 0.3972) + 
    (df$sr_band3 * 0.3904) +
    (df$sr_band4 * 0.6966) +
    (df$sr_band5 * 0.2286) +
    (df$sr_band7 * 0.1596)
  return(x)
}

greenness7 <- function(df){
  x = (df$sr_band1 *-0.3344) +
    (df$sr_band2 * -0.3544) + 
    (df$sr_band3 * -0.4556) +
    (df$sr_band4 * 0.6966) +
    (df$sr_band5 * -0.0242) +
    (df$sr_band7 * -0.2630)
  return(x)
}

wetness7 <- function(df){
  x = (df$sr_band1 * 0.2626) +
    (df$sr_band2 * 0.2141) + 
    (df$sr_band3 * 0.0926) +
    (df$sr_band4 * 0.0656) +
    (df$sr_band5 * -0.7629) +
    (df$sr_band7 * -0.5388)
  return(x)
}


# get scenes -------------------------------------------------------------------
scenes = st_transform(scenes,st_crs(plot_data)) # matching projections
scenes = scenes[scenes$ROW<100,] # getting rid of path/rows we don't want 
gb_plots = st_intersection(gb_plots,scenes[,9:10]) # grabbing only the row and path
gb_plots$path_row = as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 
# creating a handy dandy field that outputs the exact string that is in the filename

# extract landsat data to plots ------------------------------------------------

landsat_path = "/Volumes/seagate_external/internship_project/atmos_corrected_landsat/landsat_" 
dir.create("data/scrap", showWarnings = FALSE)
exdir = "data/scrap/"

years = unique(gb_plots$year) # getting the years plots were monitored - 2011-2015
path_row_combos = unique(gb_plots@data$path_row) # this gives us the unique path row combinations

# years = 2012 # for testing. comment these out when running on all the data
#path_row_combos = path_row_combos[c(1,6)] # for testing

# matching the landsat projection (it's usually easier to reproject vectors instead of rasters)
gb_plots = spTransform(gb_plots, '+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

gbplots_list = list()
counter = 1
for(i in 1:length(years)){
  for(j in 1:length(path_row_combos)){
    
    # subsetting the plots for the year and row/column combination
    print("subsetting")
    gbplots_subset = gb_plots[gb_plots$path_row == path_row_combos[j]
                              & gb_plots$year == years[i],]
    print(paste(round(counter/125*100), "%")) #progress indicator
    
    if(length(gbplots_subset) > 0) {
      # listing all the files with the right year and path/row combo
      tar_files = Sys.glob(paste0(landsat_path, years[i], "/","LE07", 
                                  path_row_combos[j],
                                  "*.tar.gz")) 
      print("untarring")
      untar(tar_files[1], exdir = exdir)
      #listing only the tif files we've extracted (there's a bunch of metadata etc too, which you should familiarize yourself with)
      tif_files = Sys.glob(paste0(exdir,"*.tif"))
      
      for(k in 1:length(tif_files)){
        # now we loop through each tif file and extract the values
        band = raster::raster(tif_files[k]) # this just loads the raster
        gbplots_subset = raster::extract(band, gbplots_subset, sp=TRUE) # and here we extract
        print(c(years[i],path_row_combos[j],tif_files[k]))
      }
      file.remove(Sys.glob(paste0(exdir,"*"))) # now we delete the tifs
      #rename the columns - this required just manually going through and figuring out where to clip the 
      #column names at
      #if we don't do this, each landsat file will spit out unique column names, making putting the data
      #back together a big pain
      colnames(gbplots_subset@data) = c(names(gbplots_subset[1:55]), 
                                        substr(names(gbplots_subset[56:69]), 42,1000))
      #adding the data to a list - converting back to a shapefile will be easy later, 
      #since they have lat and long as data frame columns
      gbplots_list[[counter]] = gbplots_subset@data
    }
    else {print("nopoints")}
    counter = counter + 1 
  }
}

gbplots_list[[1]] = gbplots_list[[1]][,c(1:69)]

df = do.call("rbind", gbplots_list) # this takes the list of stuff and makes it into a data frame
df <- df[df$pixel_qa == 66, ] #select plots without clouds

#### Create vegetation indices ####
df$NDVI <- (df$sr_band4 - df$sr_band3)/ (df$sr_band4 + df$sr_band3)
df$EVI <- 2.5 * ((df$sr_band4 - df$sr_band3)/(df$sr_band4 + (6 * df$sr_band3) - (7.5 * df$sr_band1) + 1))
df$SAVI <- ((df$sr_band4 - df$sr_band3) / (df$sr_band4 + df$sr_band3 + 0.5)) * 1.5
df$SR = df$sr_band4/df$sr_band3





write.csv(df, file = "data/plots_with_landsat.csv")


#### Slope aspect elevation -  d####
terrain_path <- '/Users/TheEagle/fire_proj/wet_dry/data/SRTM_dem/'
demfile <- "/Users/TheEagle/fire_proj/wet_dry/data/SRTM_dem/gb_dem_2"
dempath <- "/Volumes/seagate_external/internship_project/SRTM_DEM_raster/dem/"

demlist=list.files(dempath, pattern="tif$", full.names=TRUE)

for(i in demlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

raster_list <- list()
for(i in 1:length(demlist)){
  raster_list[[i]] <- raster(demlist[i])
}

gb_srtm_dem <- do.call(raster::merge, raster_list, filename = demfile)


#### terrain raster ####

sloperaster <- terrain(gb_srtm_dem, 
                       opt = 'slope', 
                       unit = 'degrees', 
                       neighbors = 8, 
                       filename = paste0(terrain_path,'slope'), format = 'GTiff')

aspectraster <- terrain(gb_srtm_dem, opt = 'aspect', unit ='degrees', 
                        neighbors = 8, 
                        filename = paste0(terrain_path,'aspect'), format = 'GTiff')

TPIraster <-  terrain(gb_srtm_dem, opt = 'TPI', 
                      filename = paste0(terrain_path,'TDI'), format = 'GTiff')

TRIraster <- terrain(gb_srtm_dem, opt = 'TRI', 
                     filename = paste0(terrain_path,'TRI'), format = 'GTiff')

roughnessraster <- terrain(gb_srtm_dem, opt = 'roughness', 
                           filename = paste0(terrain_path,'roughness'), format = 'GTiff')

rm(sloperaster, aspectraster, TPIraster, TRIraster, roughnessraster)


####successful terrain merge####
coords <- df[c("Longitude", "Latitude")]
df_sp <- SpatialPointsDataFrame(coords, df, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

df_sp@data$elevation <- raster::extract(gb_srtm_dem, df_sp)
df_sp@data$slope <- raster::extract(sloperaster, df_sp)
df_sp@data$aspect <- raster::extract(aspectraster, df_sp)
df_sp@data$TPI <- raster::extract(TPIraster, df_sp)
df_sp@data$TRI <- raster::extract(TRIraster, df_sp)
df_sp@data$roughness <- raster::extract(roughnessraster, df_sp)

df_sp@data$folded_aspect = abs(180 - abs(df_sp@data$aspect - 225))
df <- df_sp@data

#  
# gridded ssurgo?
# homogeneos versus heterogeneous
# add date visited, date of landsat
# look into landsat 4/5





# this object is like a gigabyte, so make sure to delete it from memory so it's not 
# saved as an R object in the .RData file (thus making our git pushes and pulls 
# take forever)
rm(x)

# for(i in 1:length(years)){
#   for(j in 1:length(path_row_combos)){
#     
#     
#     # subsetting the plots for the year and row/column combination
#     print("subsetting")
#     gbplots_subset = gb_plots[gb_plots$path_row == path_row_combos[j]
#                               & gb_plots$year == years[i],]
#     print(paste(round(counter/125*100), "%")) #progress indicator
#     print(c(length(gbplots_subset), years[i], path_row_combos[j]))
#     counter = counter +1
#   }}



####tasselled cap####
df$greenness <- greenness7(df)
df$brightness <- brightness7(df)
df$wetness <- wetness7(df)
#### train landsat pixels as shrub vs grass ####

# Dylan's progress starts here

#libraries
install.packages("randomForest"); install.packages("party"); 
library(randomForest); 
library(rpart); 
library(rattle); 
library(rpart.plot); 
library(RColorBrewer); 
library(caret); 
library(rfUtilities)
library(ROCR)


#load and prep data
gb_data <- read.csv("data/plots_with_landsat.csv")
gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)



## for splitting data into test and training sets, might not need this since random forests do the splitting and sampling within the function
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splits.gb_plots <- splitdf(gb_plots, seed = 1)

trainset.gb_plots <- splits.gb_plots$trainset; testset.gb_plots <- splits.gb_plots$testset




# end of Dylan's work

library(picante)

s = read.csv("data/plots_with_landsat.csv")
t = s[rowSums(s[,c(32,39)]) > 0,]
u = t[,c(32,39)]

comm <- decostand(u, method = "total")
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)
t$cluster <- as.factor(cutree(comm.bc.clust, 2))



library(randomForest)
## Random Forests using Party
library(party)
library(caret)

data = t

controls = cforest_unbiased(ntree = 500)

## Creating decision tree and forest models, replace "Class" with your dependent variable. You get a classification
## tree if this variable is categorical or a regression tree if it is continuous. Both are fine, but classification
## trees are prerhaps a little easier to interpret
tree<-ctree(cluster ~ sr_band1 +sr_band2 +sr_band3 +sr_band4 +sr_band5 + sr_band7, data= data, controls = controls) ## Fitting the decsion tree model
plot(tree) ## Plotting this model
fit <- cforest(cluster ~ ., data=data, controls = controls) ## Fitting the RF model

TreeStats<-caret:::cforestStats(tree) ## Extracting accuracy statistics for decision tree
fitStats<-caret:::cforestStats(fit) ## Extracting accuracy statistics for forest model

imp<-varimp(fit, conditional=T) ## Determining variable importance, as measured by mean decrease in accuracy of model with elimination of variable

