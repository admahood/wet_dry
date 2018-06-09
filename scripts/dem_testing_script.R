system("aws s3 sync s3://earthlab-amahood/data/dem_testing /home/rstudio/wet_dry/data")

slope_subset_stacktest <- raster("data/slope_clip.tif")

lf_dem_subset_stacktest <- raster("data/lf_dem_clip.tif")

landsat_subset_stacktest <- raster::brick("data/subset_2001.tif")

crs(slope_subset_stacktest)
crs(lf_dem_subset_stacktest)

final_crs <- crs(landsat_subset_stacktest)

projected_slope_stacktest <- projectRaster(slope_subset_stacktest, crs = final_crs)

crs(projected_slope_stacktest)

projected_lf_dem_stacktest <- projectRaster(lf_dem_subset_stacktest, crs = final_crs)

crs(projected_lf_dem_stacktest)

raster::stack(landsat_subset_stacktest, projected_lf_dem_stacktest)

extent(projected_lf_dem_stacktest)

desired_extent <- extent(landsat_subset_stacktest)
lf_dem_matchedextent <- raster::setExtent(projected_lf_dem_stacktest, desired_extent)

extent(lf_dem_matchedextent)

stack_test_2 <- raster::stack(landsat_subset_stacktest, lf_dem_matchedextent)

lf_dem_resamp <- raster::resample(lf_dem_matchedextent, landsat_subset_stacktest)

stack_test <- raster::stack(lf_dem_resamp, landsat_subset_stacktest)

stack_test

summary(stack_test)
summary(lf_dem_matchedextent)
