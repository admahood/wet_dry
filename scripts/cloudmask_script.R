system("aws s3 sync s3://earthlab-amahood/data/landsat_cloudmask_test /home/rstudio/wet_dry/data/landsat_cloudmask_test")

# import spatial packages
library(raster)
library(rgdal)
library(rgeos)
# turn off factors
options(stringsAsFactors = FALSE)

#list tar filenames in landsat_cloudmask_test
tar_list <- list.files("/home/rstudio/wet_dry/data/landsat_cloudmask_test", full.names = T)

#for loop for untarring and placing each scene's tif files into separate folder
for (i in 1:length(tar_list)) {
  dir.create(paste0("/home/rstudio/wet_dry/data/landsat_cloudmask_test", "/", "scene", i))
  untar(tar_list[i], exdir = paste0("/home/rstudio/wet_dry/data/landsat_cloudmask_test", "/", "scene", i))
}

#list tif files for each scene
tifs_1 <- list.files("/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene1", "*.tif", full.names = T)

tifs_2 <-  list.files("/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene2", "*.tif", full.names = T)

tifs_3 <-  list.files("/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene3", "*.tif", full.names = T)

#stack tif files for each scene
s1_unmasked <- stack(tifs_1)
s1 <- stack(tifs_1)


s2 <- stack(tifs_2)



s3 <- stack(tifs_3)


#create cloud mask for first scene 
mask_1 <- raster(tifs_1[10])
plot(mask_1)
mask_1[mask_1 > 0] <- NA
plot(mask_1)

#apply mask
s1 <- mask(s1, mask = mask_1)

#plot masked and unmasked scene
plotRGB(s1, 6, 5, 4, stretch = "hist")

plotRGB(s1_unmasked, 6, 5, 4, stretch = "hist")

#issues with extent..... for some reason crop won't match the extents

s1.2 <- resample(s1, s2, filename = "/home/rstudio/wet_dry/data/landsat_cloudmask_test/resamptest_1")




#uploading results to s3
writeRaster(s2, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene2_allbands")
writeRaster(s3, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene3_allbands")
writeRaster(s1, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene1_allbands")
file.rename("/home/rstudio/wet_dry/data/landsat_cloudmask_test/resamptest_1.grd", "/home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks/resamptest_1.grd")

system("aws s3 sync ~/wet_dry/data/landsat_cloudmask_test s3://earthlab-amahood/data/landsat_cloudmask_test")
