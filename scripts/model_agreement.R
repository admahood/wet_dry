#Install packages
libs <- c("dplyr", "raster", "sp")

iini <-function(x){
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#download data
system("aws s3 sync s3://earthlab-amahood/data/model_agreement data/model_agreement") #single year test data
system("aws s3 sync s3://earthlab-amahood/data/mucc_ensemble_results data/preensemble") #all years/models path 42 scene 31 (winnemucca)

#set values for sc and date
date <- paste(strsplit(date()," ")[[1]][c(2,3,5)],collapse="_")
sc_values = c("sc_9", "sc_14", "sc_21") #categories for separating pre-ensemble results into model type

#grab paths for pre-ensemble model results
model_results <- list.files("data/preensemble", full.names = T)

#create empty lists
over_grass_list <- list()
over_shrub_list <- list()
balance_list <- list()

#separate model results into lists of each model type
for(i in 1:length(model_results)) { 
if (substr(model_results[i], 36, 40) == sc_values[3]) {
  over_grass_list[i] = model_results[i]
} else if (substr(model_results[i], 36, 39) == sc_values[1]) {
  over_shrub_list[i] = model_results[i]
} else {
  balance_list[i] = model_results[i]
}
}

#remove weird NULLs in lists from above loop (probably some fix for this but this works for now)
over_grass_list <- over_grass_list[57:84]
over_shrub_list <- over_shrub_list[29:56]

#combine lists for each model category
sorted_list <- list(over_grass_list, over_shrub_list, balance_list)

#loop for grabbing each year's model results, reclassifying, and combining into ensemble for s3
for(i in 1:length(years)) {
  if (substr(as.character(sorted_list[[1]][i]), 61, 64) == years[i]) {
    over_g_raster <- raster(as.character(sorted_list[[1]][i]))
  }
  if (substr(as.character(sorted_list[[2]][i]), 61, 64) == years[i]) {
    over_s_raster <- raster(as.character(sorted_list[[2]][i]))
  }
  if (substr(as.character(sorted_list[[3]][i]), 61, 64) == years[i]) {
    balance_raster <- raster(as.character(sorted_list[[3]][i]))
  }
  matrix <- c(1, 0, 2, 1)
  rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
  
  over_grass_rclss <- reclassify(over_g_raster, rclss_matrix)
  
  over_shrub_rclss <- reclassify(over_s_raster, rclss_matrix)
  
  balanced_rclss <- reclassify(balance_raster, rclss_matrix)
  
  ensemble_agreement <- over_grass_rclss + over_shrub_rclss + balanced_rclss
  
  ensemble_filename  <- paste0("data/model_agreement/", "ensemble_", years[i], "_", date, "_", "model_results.tif")
  
  writeRaster(ensemble_agreement, ensemble_filename, overwrite = T, progress = 'text')
  
  system(paste0("aws s3 cp ", ensemble_filename, " s3://earthlab-amahood/data/mucc_ensemble_results_done/", substr(ensemble_filename, 22, 150)))
}

#### SCRAP CODE FROM WRITING ABOVE FOR LOOP (IN CASE IT DOESNT WORK)####

# for(i in 1:length(sorted_list)) {
# for(j in 1:length(sorted_list[[i]])) {
#   matrix <- c(1, 0, 2, 1)
#   rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
#   
#   over_grass_rclss <- reclassify(over_grass, rclss_matrix)
#   
#   over_shrub_rclss <- reclassify(over_shrub, rclss_matrix)
#   
#   balanced_rclss <- reclassify(balance_model, rclss_matrix)
#   
#   pred <- raster(as.character(sorted_list[[i]][j]))
#   
#   
#   }
# }
# 
# if (i == 1) {
#   over_grass_raster_list[] <- raster(as.character(sorted_list[[i]][j]))
# } else if (i == 2) {
#   over_shrub_raster_list[] <- raster(as.character(sorted_list[[i]][j]))
# } else if (i == 3) {
#   balance_raster_list[] <- raster(as.character(sorted_list[[i]][j]))
# }

#### TEST SINGLE YEAR SCRIPT (WORKS AS OF JAN 19 FOR 3 MODEL RESULTS FROM SAME YEAR) ####
over_grass <- raster("data/model_agreement/satvi_elev_sc_21_mtry_3_nodes_3_ls5_2010_042031_model_results.tif")
over_shrub <- raster("data/model_agreement/satvi_elev_sc_9_mtry_10_nodes_2_ls5_2010_042031_model_results.tif")
balance_model <- raster("data/model_agreement/satvi_elev_sc_14_mtry_5_nodes_4_ls5_2010_042031_model_results.tif")

#reclassify rasters to 0, 1 binary 
ensemble <- list(over_grass, over_shrub, balance_model)
ensemble_rclss <- list()

for (i in 1:length(ensemble)) {
  
  matrix <- c(1, 0, 2, 1)
  rclss_matrix <- matrix(matrix, ncol = 2, byrow = T)
  ensemble_rclss[[i]] <- reclassify(ensemble[[i]], rclss_matrix)
}

#add each classes overpredicting rasters and balanced raster to get an agreement layer, 0 = confident grass, 1 = likely grass, 2 = likely shrub, 3 = confident shrub
ensemble_agreement <- ensemble_rclss[[1]] + ensemble_rclss[[2]] + ensemble_rclss[[3]]

ensemble_filename  <- paste0("data/model_agreement/", "ensemble", "_", date, "_", "model_results.tif")
                                          
writeRaster(ensemble_agreement, ensemble_filename, overwrite = T, progress = 'text')

system(paste0("aws s3 cp ", ensemble_filename, " s3://earthlab-amahood/data/model_agreement/", substr(ensemble_filename, 22, 120)))
