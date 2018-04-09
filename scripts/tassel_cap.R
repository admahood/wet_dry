library(RStoolbox)
library(raster)

file = Sys.glob(paste0("data/landsat_2012/*.gz"))
exdir = "data/landsat_2012/"

untar(file, exdir = exdir)
tif_files = Sys.glob(paste0(exdir,"*.tif"))



stk = raster::stack(tif_files[8:13])
x=tasseledCap(stk, sat = "Landsat7ETM")

plot_data = readOGR(dsn = "data/BLM_AIM", layer = "BLM_AIM_20161025") #BLM plots
clip = raster::intersect(plot_data, x)
ext = raster::extract(x,clip)
