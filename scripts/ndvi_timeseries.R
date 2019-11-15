#Title: Using NDVI time series to determine proper dates for veg. index differencing
#Author(s): Dylan Murphy, Michael Koontz
#Date created: 11/15/19
#Date of last modification: 11/15/19

#### 1. Set up ####

#load packages
library(tidyverse)
library(sf)
library(lubridate)

#pull data from s3
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/ndvi_timeseries_earthengine/ data/ndvi_ts/")

####2. data exploration and plotting NDVI time series ####

#create point extraction object
ls_pts <- sf::st_read("data/ndvi_ts/ndvi-sequence_landsat5_dylans-plots.geojson")

#create attributes for date of image, satellite, image id, and point id by separating main ID tag into components
ls_pts <- 
  ls_pts %>% 
  tidyr::separate(id, into = c("satellite", "image_id", "date", "pt_id"), sep = "_") %>% 
  dplyr::mutate(date = ymd(date), 
                year = year(date), 
                month = month(date), 
                day = day(date)
                ) %>% 
  dplyr::arrange(pt_id, date)

pt_id_vec <- c()
for(i in 1:length(ls_pts$pt_id)) {
  pt_id_vec[i] <- ls_pts[i,]$pt_id
  pt_id_vec[i] <- substr(pt_id_vec[i], 19, 21)
}

ls_pts <- ls_pts %>% dplyr::mutate(pt_id = pt_id_vec)
#grab a particular point/year
ls_pt_test <- ls_pts[ls_pts$year == "2010",]
ls_pt_test <- ls_pt_test[ls_pt_test$pt_id == "00000000000000000025",]

#plot ndvi values for point/year of interest and fit a curve to them
ggplot(ls_pt_test, aes(x = date, y = ndvi_landsat5, color = Label)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()
