####6/19 code starts here. basic mask creation and application to single scene accomplished 
#as well as creation of stacks from each scene for figuring out how to replace masked values ####

# setup ------------------------------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)

# turn off factors
options(stringsAsFactors = FALSE)

# import files -----------------------------------------------------------------
system("aws s3 sync s3://earthlab-amahood/data/landsat_cloudmask_test /home/rstudio/wet_dry/data/landsat_cloudmask_test")


#list tar filenames in landsat_cloudmask_test 
# changed to Sys.glob so there weren't also directories in there
# added in stuff to pick out path-row

years <- 1984:2011
path_row_combo <- path_row_combos[1] # placeholder before loop
year = years[1] # placeholder before loop
dir.create("data/scrap")

for(year in years){}
  system(paste0("aws s3 sync s3://earthlab-amahood/data/landsat/landsat_", year,
                " data/ls5/"))

  for(path_row_combo in path_row_combos){}
    tar_path <- "/home/rstudio/wet_dry/data/ls5/"
    tar_list <- Sys.glob(paste0(tar_path, "LT05", path_row_combo,"*.gz"))
    
    qas <- data.frame(filenames = NA, value = NA, goodpix = NA, i = NA)
    for (i in 1:length(tar_list)) {
      exdir <- paste0("data/scrap/",i)
      #dir.create(exdir)
      #untar(tar_list[i], exdir = exdir)
      qas[i, 1] <- Sys.glob(paste0(exdir, "/*pixel_qa.tif"))
      x<-freq(raster(Sys.glob(paste0(exdir, "/*pixel_qa.tif")))) %>% as_tibble()
      qas[i, 2] <- x[1,1]
      qas[i, 3] <- x[1,2]
      qas[i, 4] <- i
    }
    which_i <- dplyr::arrange(qas, desc(goodpix));qas
    
#list tif files for each scene
tifs_1 <- list.files(exdir, "*.tif", full.names = T)

#stack tif files for each scene
s1<- stack(tifs_1)


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
july6_masked <- crop(july6_masked, june4_masked)


#again, check to ensure resample matched the extents

compareRaster(june4_masked, july6_masked)

#replace masked values in one raster with values from another

july6_replaced <- cover(july6_masked, june4_masked)

plotRGB(july6_replaced, stretch = "lin")
plot(july6_replaced)
