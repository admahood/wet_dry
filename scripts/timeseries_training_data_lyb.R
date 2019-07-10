#Title: Time Series Training Data w/ sagebrush and cheatgrass labels based on fire history
#Author: Dylan Murphy
#Date last modified: 7/10/19

#1. Set Up
#Install/Load Packages
libs <- c("raster", "sf", "doParallel", "dplyr", "tidyverse", "foreach")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

#S3 syncs
system("aws s3 cp s3://earthlab-amahood/data/gbd_plots_w_lyb_May31.gpkg data/gbd_plots_w_lyb.gpkg")


#2. Data Prep 
#remove points outside of esp mask and duplicate plots
gbd_lyb <- st_read("data/gbd_plots_w_lyb.gpkg") %>% arrange(OBJECTID) %>% filter(esp_mask == 1)

gbd_lyb <- gbd_lyb %>% 
  mutate(duplicated = as.numeric(duplicated(gbd_lyb$OBJECTID)),
        plot_year = as.numeric(as.character(gbd_lyb$year))) %>% 
  filter(duplicated == 0) 


#3. Create Training Data Time Series Based on Fire History

#grab unique plot IDs in vector form as an iterator
plotIDs <- unique(gbd_lyb$OBJECTID)

#initiate empty list
timeseries_list <- list()

#3.1: isolate each plot in a list to work with each training "time series" individually
for(i in 1:length(plotIDs)) { 
  plotdf <- data.frame(gbd_lyb[i,])
  timeseries_list[[i]] <- plotdf
  }

#3.2: Looping over each point in the list, create duplicates for time series and 
#change year labels to match

for (i in 1:length(timeseries_list)) {
  
  # grab last year burned
  plot_lyb <- timeseries_list[[i]]$lyb 
  
  #if the plot never burned, plot_lyb = NA and the plot gets a five year time series of shrub labels
  if(is.na(plot_lyb)) { 
    
    #get the first year to begin the time series (5 years prior to plot recording in this case)
    start_year <- timeseries_list[[i]]$plot_year - 5 
    
    #duplicate plot points within the list and change plot_year label to match time series position
    timeseries_list[[i]][2,] <- timeseries_list[[i]][1,] 
    timeseries_list[[i]][2,69] <- start_year
    
    timeseries_list[[i]][3,] <- timeseries_list[[i]][1,] 
    timeseries_list[[i]][3,69] <- start_year + 1
    
    timeseries_list[[i]][4,] <- timeseries_list[[i]][1,] 
    timeseries_list[[i]][4,69] <- start_year + 2
    
    timeseries_list[[i]][5,] <- timeseries_list[[i]][1,] 
    timeseries_list[[i]][5,69] <- start_year + 3
    
    timeseries_list[[i]][6,] <- timeseries_list[[i]][1,] 
    timeseries_list[[i]][6,69] <- start_year + 4
    
    print(paste0(timeseries_list[[i]]$OBJECTID, " did not burn during the study period"))
    
    #if the plot did burn at some point, create a time series from two years after burning up 
    #until the plot's recording, OR five years if last burn was over seven years prior to recording
    } else { 
    
    #determine if there is room for a full five year time series 
    if(timeseries_list[[i]]$plot_year - plot_lyb >= 7) { 
    
    #grab start year for time series (two years after the most recent burn)
    start_year <- timeseries_list[[i]]$plot_year - 5
    
    #create vector iterator for time series years
    years_vec <- c(start_year:(timeseries_list[[i]]$plot_year - 1))
    
    #create time series and attach proper year attributes
    for(y in 1:length(years_vec)) {
      
      timeseries_list[[i]][y + 1,] <- timeseries_list[[i]][1,]
      
      timeseries_list[[i]][y + 1,69] <- years_vec[y]
      print(paste0(timeseries_list[[i]]$OBJECTID, " has a full time series and burned at some point in the past"))
    }
    } else {
      #get start year 
      start_year <- plot_lyb + 2
        #determine if there is room for a time series at all, and if there is:
        if (timeseries_list[[i]]$plot_year - start_year > 0) {
        
        #create years vector iterator
        years_vec <- c(start_year:(timeseries_list[[i]]$plot_year - 1))
        
        #loop on years vector andcreate time series (likely truncated) and attach proper year attribute values
        for(y in 1:length(years_vec)) {
          
        timeseries_list[[i]][y + 1,] <- timeseries_list[[i]][1,]
          
        timeseries_list[[i]][y + 1,69] <- years_vec[y]
        }
        print(paste0(timeseries_list[[i]]$OBJECTID, " has a truncated time series")) 
        } else {
          
          #in the case that the most recent burn is so recent that the time series will not fit, print this
          print(paste0(timeseries_list[[i]]$OBJECTID, " burned too recently for time series")) 
        }
      }
    }
}
 
# 3.3: NEEDS UPDATING TO MATCH CORRECTED LABEL METHOD - WORK STOPPED HERE 6/4

#now that we have a list containing a time series for each data point, 
#we need to attach shrub/grass labels to them based on fire history, and convert them back 
#into one dataframe for easy writing out as a gpkg. 

#create empty dataframe for storing time series points
gbd_full_timeseries <- data.frame()

#loop over the list of different "time series", add labels based on fire history, convert into single dataframe
for(i in 1:length(timeseries_list)) {
  
  #create dataframe with each row as one year in a particular point's time series
  timeseries_point <- as.data.frame(timeseries_list[[i]]) 
  
  #grab last year burned for a particular point
  plot_lyb <- timeseries_list[[i]][1,]$lyb
  
  #create empty vector to store labels 
  label_vec <- as.character(c())
 
  #
  #if never burned, then label based on shrub cover and annual grass cover for all of time series. (assume steady state) 
  #if the plot did burn at some point, label based on shrub cover and annual grass cover (assume steady state)
  
  
  if(timeseries_point[1,]$SagebrushC > 5 & timeseries_point[1,]$InvAnnGras < 2) {
    label_vec <- rep("shrub", length(timeseries_point$plot_year))
  }else if(timeseries_point[1,]$SagebrushC < 1 & timeseries_point[1,]$InvAnnGras >4) {
    label_vec <- rep("grass", length(timeseries_point$plot_year))
 } else { label_vec <- rep("mixed", length(timeseries_point$plot_year))}
 
  
  #OLD for loop commented out below  
  #loop over each year and determine how to label it based on fire history (store labels in vector):

#   for(k in 1:length(timeseries_point$plot_year)) { 
#     label_vec[k] <- if (!is.na(plot_lyb)) {ifelse(timeseries_point[,69][k] >= plot_lyb, "grass", "shrub")} 
#     else { "shrub"}
#   }
# 
# #add label column using label vector
timeseries_point <- dplyr::mutate(timeseries_point, label = label_vec)


#add to main dataframe containing all time series points
gbd_full_timeseries <- rbind(gbd_full_timeseries, timeseries_point)
}

gbd_full_timeseries <- gbd_full_timeseries %>% mutate(cluster = as.numeric(as.factor(label)))
# 4: Save and Upload to S3
st_write(gbd_full_timeseries, "data/gbd_plots_w_lyb_timeseries_Jun5.gpkg")
system("aws s3 cp data/gbd_plots_w_lyb_timeseries_Jun5.gpkg s3://earthlab-amahood/data/training_plots_timeseries/gbd_plots_w_lyb_timeseries_Jun5.gpkg")

