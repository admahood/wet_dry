
# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

system("aws s3 sync s3://earthlab-amahood/data/BLM_AIM /home/rstudio/wet_dry/data/BLM_AIM")
system("aws s3 sync s3://earthlab-amahood/data/WRS2_paths/wrs2_asc_desc /home/rstudio/wet_dry/data/WRS2_paths/wrs2_asc_desc")
system("aws s3 sync s3://earthlab-amahood/data/ecoregions /home/rstudio/wet_dry/data/ecoregions")


plot_data <- st_read("data/BLM_AIM/BLM_AIM_20161025.shp") #BLM plots
ecoregions <- st_read("data/ecoregions/NA_CEC_Eco_Level3.shp") #ecoregions
scenes <- st_read("data/WRS2_paths/wrs2_asc_desc/wrs2_asc_desc.shp") #landsat scenes

great_basin <- subset(ecoregions, NA_L3NAME %in% c("Northern Basin and Range", 
                                                   "Central Basin and Range", 
                                                   "Snake River Plain"))

great_basin <- st_transform(great_basin, st_crs(plot_data)) # matching projections
great_basin <- st_union(great_basin) # dissolve into one polygon
gb_plots <- st_intersection(plot_data,great_basin)# clipping to great basin
gb_plots <- gb_plots[!is.na(gb_plots$DateVisite),] # removing rows where the plot was not read
gb_plots$year <- substr(as.character(gb_plots$DateVisite),1,4) # making a year field


# get scenes -------------------------------------------------------------------
scenes <- st_transform(scenes,st_crs(plot_data)) # matching projections
scenes <- scenes[scenes$ROW<100,] # getting rid of path/rows we don't want 
gb_plots <- st_intersection(gb_plots,scenes[,9:10]) # grabbing only the row and path
gb_plots$path_row <- as.character(paste0("0",gb_plots$PATH,"0",gb_plots$ROW)) 
years <- unique(gb_plots$year) # getting the years plots were monitored - 2011-2015

path_row_years <- unique(paste(gb_plots$year,gb_plots$path_row, sep="_")) %>%
  as_tibble()

done <- system("aws s3 ls s3://earthlab-amahood/data/landsat7_pixel_replaced/", intern = TRUE) %>%
  strsplit("_")
done <- done[-1]
for(i in 1:length(done)){
  done[[i]] <- done[[i]][2:3]
  done[[i]] <- paste(done[[i]], collapse = "_")
}
done <- unlist(done) %>%
  as_tibble() %>%
  mutate(done = as.factor("yes")) %>%
  right_join(path_row_years)

