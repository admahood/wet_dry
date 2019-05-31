#Title: Time Series Training Data w/ sagebrush and cheatgrass labels based on fire history
#Author: Dylan Murphy
#Date last modified: 5/31/19

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


#grab unique plot IDs in vector form as an iterator
plotIDs <- unique(gbd_lyb$OBJECTID)

#initiate empty list
timeseries_list <- list()

#isolate each plot in a list 
for(i in 1:length(plotIDs)) { 
  plotdf <- data.frame(gbd_lyb[i,])
  timeseries_list[[i]] <- plotdf
  }

for (i in 1:length(timeseries_list)) {
  
  plot_lyb <- timeseries_list[[i]]$lyb
  
  if(is.na(plot_lyb)) { 
    start_year <- timeseries_list[[i]]$plot_year - 5
    
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
    
    dplyr::mutate(timeseries_list[[i]], label = "shrub")
    } else {
    start_year <- plot_lyb - 5 
    
    years_vec <- c(start_year:timeseries_list[[i]]$plot_year)
    
    for(y in 1:length(years_vec)) {
      
      timeseries_list[[i]][y + 1,] <- timeseries_list[[i]][1,]
    
      timeseries_list[[i]][y + 1,69] <- years_vec[y]
    }
    
    label_vec = for(yy in 1:length(years_vec)) { 
      ifelse(timeseries_list[[i]][yy, 69] >= plot_lyb, "grass", "shrub")
    }
    
    dplyr::mutate(timeseries_list[[i]], label = label_vec)
  }
}
   
