####6/19 code starts here. basic mask creation and application to single scene accomplished 
#as well as creation of stacks from each scene for figuring out how to replace masked values ####

# import spatial packages
library(raster)
library(rgdal)
library(rgeos)
# turn off factors
options(stringsAsFactors = FALSE)

system("aws s3 sync s3://earthlab-amahood/data/landsat_cloudmask_test /home/rstudio/wet_dry/data/landsat_cloudmask_test")


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

#issues with extent....use resample instead of crop/extend to avoid memory allocation error

s1.2 <- resample(s1, s2, filename = "/home/rstudio/wet_dry/data/landsat_cloudmask_test/resamptest_1")




#uploading results to s3
writeRaster(s2, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene2_allbands")
writeRaster(s3, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene3_allbands")
writeRaster(s1, "/home/rstudio/wet_dry/data/landsat_cloudmask_test/scene1_allbands")
file.rename("/home/rstudio/wet_dry/data/landsat_cloudmask_test/resamptest_1.grd", "/home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks/resamptest_1.grd")

system("aws s3 sync ~/wet_dry/data/landsat_cloudmask_test s3://earthlab-amahood/data/landsat_cloudmask_test")



####6/20 starts here####

#download and load stacked scenes

system("aws s3 sync s3://earthlab-amahood/data/landsat_cloudmask_test/stacks /home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks")

june4 <- brick("/home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks/scene1_allbands.gri")
june20 <- brick("/home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks/scene2_allbands.gri")
july6 <- brick("/home/rstudio/wet_dry/data/landsat_cloudmask_test/stacks/scene3_allbands.gri")


#plot to visualize cloud cover
plotRGB(june4, r = 6, g = 5, b = 4, stretch = "hist")

plotRGB(june20, r = 6, g = 5, b = 4, stretch = "hist")

plotRGB(july6, r = 6, g = 5, b = 4, stretch = "hist")


#create pixel quality rasters for masking

pqa_june4 <- june4[[1]]
pqa_june20 <- june20[[1]]
pqa_july6 <- july6[[1]]


plot(pqa_june4)
plot(pqa_june20)
plot(pqa_july6)

#drop unnecessary layers from stack to speed up processing a bit
june4 <- dropLayer(june4, c(1, 2, 3, 10))
june4 <- brick(june4)
june20 <- dropLayer(june20, c(1, 2, 3, 10))
june20 <- brick(june20)
july6 <- dropLayer(july6, c(1, 2, 3, 10))
july6 <- brick(july6)

#create generic mask creation function for use in overlay (can be done with basic raster arithmetic but overlay is easier on memory)
ls5_maskcreate<- function(x, y){
  x[y != 66] <- NA
  return(x)
}

#apply mask using overlay()

june4_masked <- overlay(x = june4, y = pqa_june4, fun = ls5_maskcreate)
july6_masked <- overlay(x = july6, y = pqa_july6, fun = ls5_maskcreate)

plotRGB(june4_masked, stretch = "hist")
plotRGB(july6_masked, stretch = "hist")

#check to see if masked rasters have same extent

compareRaster(june4_masked, july6_masked)

#extents do not match exactly. resolve this with resample

july6_masked <- resample(july6_masked, june4_masked)

#again, check to ensure resample matched the extents

compareRaster(june4_masked, july6_masked)

#replace masked values in one raster with values from another

july6_replaced <- cover(july6_masked, june4_masked)

plotRGB(july6_replaced, stretch = "lin")
plot(july6_replaced)
