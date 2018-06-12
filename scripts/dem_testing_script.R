#DO THIS FIRST
system("aws s3 sync s3://earthlab-amahood/data/dem_testing /home/rstudio/wet_dry/data")

####June 8 work. ignore this, using for reference####
# slope_subset_stacktest <- raster("data/slope_clip.tif")
# 
# lf_dem_subset_stacktest <- raster("data/lf_dem_clip.tif")
# 
# landsat_subset_stacktest <- raster::brick("data/subset_2001.tif")
# 
# crs(slope_subset_stacktest)
# crs(lf_dem_subset_stacktest)
# 
# final_crs <- crs(landsat_subset_stacktest)
# 
# projected_slope_stacktest <- projectRaster(slope_subset_stacktest, crs = final_crs)
# 
# crs(projected_slope_stacktest)
# 
# projected_lf_dem_stacktest <- projectRaster(lf_dem_subset_stacktest, crs = final_crs)
# 
# crs(projected_lf_dem_stacktest)
# 
# raster::stack(landsat_subset_stacktest, projected_lf_dem_stacktest)
# 
# extent(projected_lf_dem_stacktest)
# 
# desired_extent <- extent(landsat_subset_stacktest)
# lf_dem_matchedextent <- raster::setExtent(projected_lf_dem_stacktest, desired_extent)
# 
# extent(lf_dem_matchedextent)
# 
# stack_test_2 <- raster::stack(landsat_subset_stacktest, lf_dem_matchedextent)
# 
# lf_dem_resamp <- raster::resample(lf_dem_matchedextent, landsat_subset_stacktest)
# 
# stack_test <- raster::stack(lf_dem_resamp, landsat_subset_stacktest)
# 
# stack_test
# 
# summary(stack_test)
# summary(lf_dem_matchedextent)

#### June 12 Work ####
####load data####
lf_dem <- raster("data/LF_DEM_GB.tif")

landsat_stacktest <- raster::brick("data/subset_2001.tif")

cropped_notprojected <- crop(lf_dem, landsat_stacktest)

#### reproject DEM to match landsat crs ####
final_crs <- crs(landsat_stacktest)

prj_lf_dem <- projectRaster(lf_dem, crs = final_crs)

####resample to match resolution and extent ####
resamp_lf_dem <- resample(prj_lf_dem, landsat_stacktest, method = 'bilinear')

####stack rasters ####
stacked_landsatdem <- raster::stack(landsat_stacktest, resamp_lf_dem)

####plots####
plotRGB(landsat_stacktest, r = 3, g = 2, b = 1, stretch = "hist")
plot(prj_lf_dem, add = T, alpha = .8)

plot(resamp_lf_dem, add = T, alpha = .8)

raster::plot(stacked_landsatdem, 7)

plot(st_geometry(great_basin), add = TRUE)

plot(resamp_full_dem)

####creating full gb dem in proper resolution for future stacking with landsat scenes ####
#writeRaster(prj_lf_dem, "reproj_lf_dem_gb.tif")

#system("aws s3 cp /home/rstudio/wet_dry/reproj_lf_dem_gb.tif s3://earthlab-amahood/data/dem_testing/reproj_lf_dem_gb.tif")