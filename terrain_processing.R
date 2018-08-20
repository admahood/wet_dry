# nts -- clip to extent of the great basin polygon, then make aspect, elevation, etc

download.file("https://landfire.gov/bulk/downloadfile.php?FNAME=US_Topo_2010-US_DEM_Elevation.zip&TYPE=landfire",
              destfile = "data/dem_cus.tif")
