# libraries ---------------------------------------------------------------
libs <- c("sf", "tidyverse", "raster", "rgdal", "rgeos", "foreach", "doParallel")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#s3 sync 

#cropping and stacking loop 