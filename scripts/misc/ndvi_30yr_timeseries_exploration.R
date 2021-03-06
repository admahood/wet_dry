#Title: Using NDVI time series to determine proper dates for veg. index differencing
#Author(s): Dylan Murphy, Michael Koontz
#Date created: 11/15/19
#Date of last modification: 11/22/19

#Notes: Be sure to use the code chunks throughout the script specific to the satellite 
#       from which the extractions were made throughout the script

#### 1. Set up ####

#load packages

if (!"eseis" %in% rownames(installed.packages())) install.packages("eseis")
library(tidyverse)
library(sf)
library(lubridate)
library(eseis)

#pull data from s3
system("aws s3 sync s3://earthlab-amahood/wet_dry/input_vector_data/ndvi_timeseries_earthengine/ data/ndvi_ts/")

dir.create("data/date_of_max_ndvi_csv/")
#### 2. data prep ####

#create point extraction object
#LANDSAT NDVI TIME SERIES
ls_pts <- sf::st_read("data/ndvi_ts/ndvi-sequence_landsat5_dylans-plots.geojson")

#MODIS AQUA NDVI TIME SERIES
ls_pts <- sf::st_read("data/ndvi_ts/ndvi-sequence_modis_aqua_dylans-plots.geojson")

# #MODIS TERRA NDVI TIME SERIES
ls_pts <- sf::st_read("data/ndvi_ts/ndvi-sequence_modis_terra_dylans-plots.geojson")

#### USE THIS FOR LANDSAT EXTRACTIONS####

#seperate ID attribute into component parts (satellite, image ID, date, point ID) and arrange by point ID and date
ls_pts <- 
  sf::st_read("data/ndvi_ts/ndvi-sequence_landsat5_dylans-plots.geojson") %>% 
  tidyr::separate(id, into = c("satellite", "image_id", "date", "pt_id"), sep = "_") %>% 
  dplyr::mutate(date = ymd(date), 
                year = year(date), 
                month = month(date), 
                day = day(date)
                ) %>% 
  dplyr::arrange(pt_id, date)%>% 
  dplyr::mutate(pt_id = substr(pt_id, 19, 21))%>%
  dplyr::mutate(julian_day = yday(date))

#remove extra zeroes from pt ID
ls_pts <- ls_pts %>% dplyr::mutate(pt_id = substr(ls_pts$pt_id, 19, 21))
# 
# #create julian day variable for averaging max NDVI dates across classes for each year
ls_pts <- ls_pts %>% dplyr::mutate(julian_day = yday(ls_pts$date))

#### USE THIS FOR MODIS####
#seperate id attribute into component parts (year, month, day, point ID)
ls_pts <- ls_pts %>% 
tidyr::separate(id, into = c("year", "month", "day", "pt_id"), sep = "_")

#create vector of dates from year, month, day attributes
date_vec <- c()
for(i in 1:length(ls_pts$pt_id)) {
  date_vec[i] = paste0(ls_pts$year[i], ls_pts$month[i], ls_pts$day[i]) 
}

#attach full year-month-day attribute to points, format as a date data type, arrange points by ID and date
ls_pts <- ls_pts %>% 
  dplyr::mutate(date = date_vec) %>% 
  dplyr::mutate(date = ymd(date_vec)) %>%
  dplyr::arrange(pt_id, date)

#remove extra zeroes from pt ID
ls_pts <- ls_pts %>% dplyr::mutate(pt_id = substr(ls_pts$pt_id, 19, 21))

#create julian day variable for averaging max NDVI dates across classes for each year
ls_pts <- ls_pts %>% dplyr::mutate(julian_day = yday(ls_pts$date))

#### 3. Use NDVI time series to discover peak greenness date by class####
# (for selection of ideal landsat ARD scenes for veg index differencing)

#create "class" and "year" vectors
classes <- c("grass", "shrub")

#LANDSAT YEARS (if using landsat extracted NDVI)
years <- c(1984:2011)

#MODIS YEARS(if using modis extracted NDVI)
years <- c(2002:2019)

#create empty vectors to store values for year, class, and average date of maximim NDVI 
main_year_vec <- c()
main_class_vec <- c()
main_date_vec <- c()

counter <- 1

# loops for extracting average maximum ndvi for each class and year - use the appropriate loop for 
# the satellite from which NDVI values were extracted

#LANDSAT
for(i in 1:length(classes)) {
  target_class <- classes[i]
  class_group <- ls_pts[ls_pts$Label == classes[i],]
    for(j in 1:length(years)) {
      year <- years[j]
      class_year <- class_group[class_group$year == year,]
      pt_ids <- unique(class_year$pt_id)
      max_ndvi_date_vec <- c()
        for(k in 1:length(pt_ids)) {
          target_pt <- class_year[class_year$pt_id == pt_ids[k],]
          max_ndvi_pt <- target_pt[target_pt$ndvi_landsat5 == max(target_pt$ndvi_landsat5),]
          max_ndvi_date_vec[k] <- max_ndvi_pt$julian_day
          avg_max_ndvi_date <- mean(max_ndvi_date_vec)
        }
      print(paste0("average max ndvi date found for: ", target_class, " - ", year))
      main_date_vec[counter] <- avg_max_ndvi_date
      main_year_vec[counter] <- year
      main_class_vec[counter] <- target_class
      print(paste0("max ndvi date, year, and class added to appropriate vectors: ", target_class, " - ", year))
      counter <- counter + 1
}
}

#MODIS AQUA
for(i in 1:length(classes)) {
  target_class <- classes[i]
  class_group <- ls_pts[ls_pts$Label == classes[i],]
  for(j in 1:length(years)) {
    year <- years[j]
    class_year <- class_group[class_group$year == year,]
    pt_ids <- unique(class_year$pt_id)
    max_ndvi_date_vec <- c()
    for(k in 1:length(pt_ids)) {
      target_pt <- class_year[class_year$pt_id == pt_ids[k],]
      max_ndvi_pt <- target_pt[target_pt$ndvi_aqua == max(target_pt$ndvi_aqua),]
      max_ndvi_date_vec[k] <- max_ndvi_pt$julian_day
      avg_max_ndvi_date <- mean(max_ndvi_date_vec)
      print(paste0("average max ndvi date found for: ", target_class, " - ", year))
    }
    main_date_vec[counter] <- avg_max_ndvi_date
    main_year_vec[counter] <- year
    main_class_vec[counter] <- target_class
    print(paste0("max ndvi date, year, and class added to appropriate vectors: ", target_class, " - ", year))
    counter <- counter + 1
  }
}

#MODIS TERRA
for(i in 1:length(classes)) {
  target_class <- classes[i]
  class_group <- ls_pts[ls_pts$Label == classes[i],]
  for(j in 1:length(years)) {
    year <- years[j]
    class_year <- class_group[class_group$year == year,]
    pt_ids <- unique(class_year$pt_id)
    max_ndvi_date_vec <- c()
    for(k in 1:length(pt_ids)) {
      target_pt <- class_year[class_year$pt_id == pt_ids[k],]
      max_ndvi_pt <- target_pt[target_pt$ndvi_terra == max(target_pt$ndvi_terra),]
      max_ndvi_date_vec[k] <- max_ndvi_pt$julian_day
      avg_max_ndvi_date <- mean(max_ndvi_date_vec)
      print(paste0("average max ndvi date found for: ", target_class, " - ", year))
    }
    main_date_vec[counter] <- avg_max_ndvi_date
    main_year_vec[counter] <- year
    main_class_vec[counter] <- target_class
    print(paste0("max ndvi date, year, and class added to appropriate vectors: ", target_class, " - ", year))
    counter <- counter + 1
  }
}

#combine vectors of classes, years, and average max ndvi dates into one data frame
max_ndvi_df <- data.frame(main_year_vec, main_class_vec, main_date_vec)

#round dates to nearest day and add a column with non-julian dates (mm--dd--yyy)
max_ndvi_df <- max_ndvi_df %>% 
  dplyr::mutate(main_date_vec = round(max_ndvi_df$main_date_vec)) 
max_ndvi_df <- max_ndvi_df %>%
  dplyr::mutate(date = time_convert(max_ndvi_df$main_date_vec, year = max_ndvi_df$main_year_vec, output = "yyyy-mm-dd"))

#attach more understandable names to each column
names(max_ndvi_df) <- c("year", "class", "max_ndvi_julian", "max_ndvi_date")

#save landsat time series
write_csv(max_ndvi_df, path = "data/date_of_max_ndvi_csv/date_of_max_ndvi_landsat.csv")

#save aqua time series
write_csv(max_ndvi_df, path = "data/date_of_max_ndvi_csv/date_of_max_ndvi_modis_aqua.csv")

#save terra time series
write_csv(max_ndvi_df, path = "data/date_of_max_ndvi_csv/date_of_max_ndvi_modis_terra.csv")

#push all csv files to s3
system("aws s3 sync data/date_of_max_ndvi_csv s3://earthlab-amahood/wet_dry/non_spatial_data/date_of_max_ndvi")

#### 4. plotting results ####

# PLOT NDVI VALUES FOR A GIVEN POINT/YEAR #

#grab a particular point/year
ls_pt_test <- ls_pts[ls_pts$year == "2010",]
ls_pt_test <- ls_pt_test[ls_pt_test$pt_id == "24",]

#plot ndvi values for point/year of interest and fit a curve to them
ggplot(ls_pt_test, aes(x = date, y = ndvi_landsat5, color = Label)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()

# PLOT MEAN DATE OF MAXIMUM NDVI FOR EACH CLASS ACROSS TIME SERIES #

ggplot(max_ndvi_df, aes(x = year, y = max_ndvi_julian, color = class)) +
  geom_point() 


#### 5. doing some gam stuff ---------------------------------------------------

# reading in the points with landsat ndvi
ls_pts <- 
  sf::st_read("data/ndvi_ts/ndvi-sequence_landsat5_dylans-plots.geojson") %>% 
  tidyr::separate(id, into = c("satellite", "image_id", "date", "pt_id"), sep = "_") %>% 
  dplyr::mutate(date = ymd(date), 
                year = year(date), 
                month = month(date), 
                day = day(date)
  ) %>% 
  dplyr::arrange(pt_id, date)%>% 
  dplyr::mutate(pt_id = substr(pt_id, 19, 21))%>%
  dplyr::mutate(julian_day = yday(date))


years<-1984:2011
peaks <- list()
for(y in 1:length(years)){
  
  # here is where I'm splitting the grass and shrub points and selecting the 
  # year
  grass_slice <- ls_pts %>% 
    filter(year == years[y], Label == "grass") %>% 
    st_set_geometry(NULL) %>%

    # and here is the gam model (which actually a loess but whatever same basic thing).
    # I'm using the defaults. there's ways to optimize it to minimize the error
    # but i didn't do that
    mutate(loess = loess(ndvi_landsat5~julian_day, .)%>%predict)
  
  # same thing here
  shrub_slice <- ls_pts %>% 
    filter(year == years[y], Label == "shrub")%>% 
    st_set_geometry(NULL)%>%
    mutate(loess = loess(ndvi_landsat5~julian_day, .)%>%predict)
  
  # putting them back together
  slice<- rbind(grass_slice, shrub_slice) 
  
  # creating separate model objects for each plot type
  gmod <- loess(ndvi_landsat5~julian_day, grass_slice)
  smod <- loess(ndvi_landsat5~julian_day, shrub_slice)
  
  # then creating a data frame with each day as one column, and the 
  # loess predictions plus their standard errors as separate columns
  x <- data.frame(julian_day = 1:365) %>%
    mutate(loess_g = predict(gmod, newdata = julian_day),
           se_g = predict(gmod, newdata = julian_day,se=T)$se.fit,
           loess_s = predict(smod, newdata = julian_day),
           se_s = predict(smod, newdata = julian_day,se=T)$se.fit,
           
           # here is where I'm trying to get columns that i can use to maximize or 
           # minimize difference between shrub and grass
           difference = loess_g-loess_s,
           se_overlap = (se_s*2)+loess_s > loess_g-(se_g*2)) %>%
    na.omit
  
  # picking out the maximum green grass day
  max_g_day <- x[x$loess_g == max(x$loess_g, na.rm=T),
                           "julian_day"] %>%
    unique
  
  #picking out the earliest day where the ndvi starts getting close...
  # this can probably be tweaked more
  earliest_same <- x %>%
    filter(julian_day > max_g_day,
           difference < 0.03 )
    
  earliest_same <- min(earliest_same$julian_day)
  
  # this is the difference at the max green day, might be useful info
  difference <- x[x$julian_day == max_g_day,"loess_g"] - 
    x[x$julian_day == max_g_day,"loess_s"]
  
  # here is where i'm putting all the useful info into a data fram to be
  # rbinded later
  peaks[[y]] <- data.frame(
    max_g_doy = max_g_day,
    earliest_same = earliest_same,
    difference_at_peak = difference,
    max_diff = max(x$difference),
    year = years[y])
}
peaks_df<-do.call("rbind", peaks) %>%
  mutate(es_date = as.Date(paste0(year, earliest_same), "%Y%j"),
         max_g_date = as.Date(paste0(year, max_g_doy), "%Y%j"))

write_csv(peaks_df, "data/key_dates.csv")

ls_pts <- left_join(ls_pts, peaks_df, by = "year")
ggplot(ls_pts, aes(x = julian_day, y =ndvi_landsat5, color = Label)) +
  geom_smooth() +
  geom_vline(aes(xintercept= max_g_doy))+
  geom_vline(aes(xintercept = earliest_same), color = "blue")+
  facet_wrap(~year)
