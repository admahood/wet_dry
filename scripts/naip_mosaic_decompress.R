libs <- c("gdalUtils", "rgdal")

lapply(libs, library, character.only = T, verbose = F)

system("aws s3 sync s3://earthlab-amahood/data/naip/naip_county_mosaics data/naip_mosaics")

naip_mosaic_path <- "data/naip_mosaics/humboldt/ortho_1-2_1n_s_nv013_2010_1.sid"

r <- gdal_translate(naip_mosaic_path, "data/naip_mosaics/humboldt/ortho_1-2_1n_s_nv013_2010_1.tif", output_Raster = T)
