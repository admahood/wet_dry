#Script Title: randomForest model training with precipitation anomaly data & two methods of class labelling
#Date Last Edited: 2/3/19
#Author(s): Dylan Murphy, Adam Mahood

#### 1: Load Packages/Source scripts/Set seed ####

#load packages 
libs <- c("randomForest", "dplyr","sf", "caTools", "dplyr", "caret", "VSURF")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#OPTIONAL: install packages if missing:
#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE) # - optional line to install packages if missing

#load in user defined functions
source("scripts/functions.R")

#set random number seed to ensure reproducible models
set.seed(11)

#### 2: Load Training Data for Individual Years & Combine if Desired ####

#create path object
training_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_ndvi_informed"

#download training data from s3
system(paste0("aws s3 sync ", training_s3_path, " data/training_timeseries")) # Use May 21 splits (most recent)

#create object for 2010 points
gtrain2010 <- st_read("data/training_timeseries/manual_points_2class_2010_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#create object for 2009 points
gtrain2009 <- st_read("data/training_timeseries/manual_points_2class_2009_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#create object for 2008 points
gtrain2008 <- st_read("data/training_timeseries/manual_points_2class_2008_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#create object for2007 points
gtrain2007 <- st_read("data/training_timeseries/manual_points_2class_2007_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#create object for 2006 points
gtrain2006 <- st_read("data/training_timeseries/manual_points_2class_2006_ard_new_climate_vars_Jan3.gpkg") %>% st_set_geometry(NULL) %>% dplyr::select(-OBJECTID, -lyb, -path_row)

#combine 2006-2010 (or any combination of the five years) into one set of training point "time series"
  #comment out gtrain objects to select different years when testing models
gtrain <- rbind(gtrain2010, gtrain2009, gtrain2008, gtrain2007, gtrain2006)

#### 3: Create Training Data with Desired Variables####
#### 3.1: Select initial variables desired & create additional variables using user functions ####
gtrain <- gtrain  %>%
  #comment out any variables to remove them for model testing. Variables are organized by type
  dplyr::select(
    #Landsat raw bands (spring & summer)
                spring_sr_band1, spring_sr_band2, spring_sr_band3, spring_sr_band4, spring_sr_band5, spring_sr_band7,
                summer_sr_band1, summer_sr_band2, summer_sr_band3, summer_sr_band4, summer_sr_band5, summer_sr_band7,
    #Veg. indices (spring & summer)
                spring_ndvi, spring_evi, spring_savi, spring_sr, spring_ndti, spring_sla_index, spring_ndi7, spring_green_ndvi, 
                summer_ndvi, summer_evi, summer_savi, summer_sr, summer_ndti, summer_sla_index, summer_ndi7, summer_green_ndvi,
    #Tasseled cap transformations 
                spring_greenness, spring_brightness, spring_wetness, summer_greenness, summer_brightness, summer_wetness,
    #Terrain variables
                elevation, slope, aspect, 
                tpi, tri, roughness, flowdir,
    #Differenced Veg. indices
                diff_evi, diff_ndsvi, diff_savi, diff_ndvi, diff_satvi, diff_sr, 
                diff_ndti, diff_green_ndvi, diff_sla_index, diff_ndi7,
    #Climate variables
                precip_anomaly, jan_precip, feb_precip, mar_precip, apr_precip, may_precip,
                jun_precip, jul_precip, aug_precip, sep_precip, oct_precip, nov_precip, dec_precip,
                winter_precip, spring_precip, summer_precip, fall_precip,
                aet_z, def_z, tmn_z,
    #Class labels (do not remove from training data)
                Label) %>%
  dplyr::mutate(spring_ndsvi = get_ndsvi(band3 = gtrain$spring_sr_band3, band5 = gtrain$spring_sr_band5),
                summer_ndsvi = get_ndsvi(band3 = gtrain$summer_sr_band3, band5 = gtrain$summer_sr_band5),
                spring_satvi = get_satvi(band3 = gtrain$spring_sr_band3, band5 = gtrain$spring_sr_band5, band7 = gtrain$spring_sr_band7, L = 0.5),
                summer_satvi = get_satvi(band3 = gtrain$summer_sr_band3, band5 = gtrain$summer_sr_band5, band7 = gtrain$summer_sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = gtrain$aspect),
                
  ) %>%
  dplyr::select(-aspect)

#### 3.2 Variable Trimming/Model Tuning (IN PROGRESS) ####
#### 3.2.1: Adam's Model Tuning ####
# ddd is a list of data frames
# mods is a list of models
# each iteration drops the least important variable from the data frame and
# trains a new model, continuing until nothing is left

#create empty lists
ddd <- list()
mods <-list()

#create initial training dataframe
ddd[[1]] <- gtrain%>% 
  filter(Year > 2009 ) %>%
  dplyr::select(-Year)
rvars <- ncol(ddd[[1]])-2

#variable dropping loop. this takes a few minutes to run
for (i in 1:rvars){ 
  #get initial start time for progress checks
  t0<-Sys.time()
  
  #train a model with selected variables minus the dropped variable for each iteration
  mods[[i]] <- randomForest(Label ~ .,
                            data = ddd[[i]] , 
                            ntree = 1000, 
                            mtry = 10, 
                            nodesize = 4)
  
  #store resulting accuracy in var_results dataframe
  var_results[i, 1] <- mods[[i]]$err.rate[100,1]
  var_results[i, 2] <- i
  
  #find name of least important variable
  least_important <- mods[[i]]$importance %>%
    as_tibble(rownames = "var") %>%
    filter(MeanDecreaseGini == min(.$MeanDecreaseGini))%>%
    dplyr::select(var) %>%
    pull()
  
  #drop least important variable and make training dataframe for next iteration
  vvv <- least_important
  ddd[[i+1]] <- dplyr::select(ddd[[i]],-vvv)
  
  #store name of variable dropped for each iteration in dataframe
  var_results[i, 3] <- vvv
  
  #print progress, accuracy, and variable dropped to console
  print(paste("Progress:", round(i/rvars*100), "% |",
              "Accuracy:",  round(max(mods[[i]]$results$Accuracy)*100),
              "% | Dropped", vvv, Sys.time()-t0))
}

#view results of variable dropping on model accuracy 
var_results 

#plot accuracy ~ number of variables dropped to quickly find sweet spot for # of vars
ggplot(var_results, aes(x=nvars-1, y=accuracy)) + 
  geom_line()

#### 3.2.1 VSURF Model Tuning ####
#remove NAs from gtrain
gtrain_noNA <- na.omit(gtrain)

#store veg class labels in a seperate object
labels <- gtrain_noNA$Label

#remove labels from gtrain
gtrain_noNA <- gtrain_noNA %>% dplyr::select(-Label)

#find number of cores to use in parallel VSURF 
ncores<-detectCores()-1

#run VSURF variable selection
model2.vsurf <- VSURF(gtrain_noNA, labels, ncores=ncores,parallel = T)

#### 4: Random Forest Model Training ####

#Train random forest model
model2 <- randomForest(Label ~ . ,
                      data = gtrain, 
                      ntree = 5000, 
                      mtry = 10, 
                      nodesize = 4,
                      na.action = na.exclude #make sure to remove any rows with NAs present
)
#### END ####

