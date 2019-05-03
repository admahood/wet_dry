libs <- c("randomForest", "tidyverse","sf", "foreach", "doParallel",
          "caTools", # for sample.split()
          "caret", # for confusionMatrix
          "ranger",
          "gganimate",
          "raster",
          #"meteo", #for tiling
          "spdep", #for the weights
          "rfUtilities" # for multi.collinear()
)

lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)



system("aws s3 sync s3://earthlab-amahood/data/naip data/naip")
system("aws s3 sync s3://earthlab-amahood/data/PRISM_precip_annual data/precip_annual")
system("aws s3 sync s3://earthlab-amahood/data/ls5_mucc_2011 data/ls5")

naip <- raster("data/naip/m_4011703_ne_11_1_20100704.tif")
# naip<-list()
# naip[[1]] <- raster("data/naip/m_4011703_ne_11_1_20100704.tif") #wmuc
# naip[[2]] <- raster("data/naip/n_4111761_nw_11_1_20060813.tif") #frank
# naip[[3]] <- raster("data/naip/m_4111823_sw_11_1_20100628.tif")

ls5 <- stack(list.files("data/ls5", full.names = T))
#ls5 <- crop(ls5, naip)

#reprojecting, cropping, resampling PRISM annual precip to 30m resolution for use as predictor variables
prism <- list()
folders <- list.files("data/precip_annual/PRISM_annual_precip", full.names = T)
for (i in 1:length(folders)) {
  prism_oneyr <- raster(list.files(folders[i], pattern = "\\.bil$", full.names = T))
  prism_oneyr <- projectRaster(prism_oneyr, ls5, res = 30)
  prism_oneyr <- crop(prism_oneyr, naip)
  prism[i] <- prism_oneyr
}
