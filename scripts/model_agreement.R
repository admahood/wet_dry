date <- paste(strsplit(date()," ")[[1]][c(2,3,5)],collapse="_")

system("aws s3 sync s3://earthlab-amahood/data/model_agreement data/model_agreement")

system("aws s3 sync s3://earthlab-amahood/data/mucc_ensemble_results data/preensemble")


model_results <- list.files("data/preensemble")

over_grass_list <- list()
over_shrub_list <- list()
balance_list <- list()

year_list <- list()


years <- as.character(c(1984:2011))
for(i in 1:length(years)) {
  if(substr(model_results[j], 44, 47) == i) {
    
  }
}




if(model_results[i])



over_grass <- raster("data/model_agreement/satvi_elev_sc_21_mtry_3_nodes_3_ls5_2010_042031_model_results.tif")
over_shrub <- raster("data/model_agreement/satvi_elev_sc_9_mtry_10_nodes_2_ls5_2010_042031_model_results.tif")
balance_model <- raster("data/model_agreement/satvi_elev_sc_14_mtry_5_nodes_4_ls5_2010_042031_model_results.tif")


#reclassify rasters to 0, 1 binary 
matrix <- c(1, 0, 2, 1)
rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)

over_grass_rclss <- reclassify(over_grass, rclss_matrix)

over_shrub_rclss <- reclassify(over_shrub, rclss_matrix)

balanced_rclss <- reclassify(balance_model, rclss_matrix)

#add each classes overpredicting rasters and balanced raster to get an agreement layer, 0 = confident grass, 1 = likely grass, 2 = likely shrub, 3 = confident shrub
ensemble_agreement <- over_grass_rclss + over_shrub_rclss + balanced_rclss

ensemble_filename  <- paste0("data/model_agreement/", "ensemble", "_", date, "_", "model_results.tif")
                                          
writeRaster(ensemble_agreement, ensemble_filename, overwrite = T, progress = 'text')

system(paste0("aws s3 cp ", ensemble_filename, " s3://earthlab-amahood/data/model_agreement/", substr(ensemble_filename, 22, 120)))


#for loop for reclassification for some reason only returning third item of list, will fix later -----

#ensemble <- list(over_grass, over_shrub, balance_model)



# for (i in 1:length(ensemble)) {
#   ensemble_rclss <- list()
#   matrix <- c(1, 0, 2, 1)
#   rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
#   ensemble_rclss[[i]] <- reclassify(ensemble[[i]], rclss_matrix)
# }
