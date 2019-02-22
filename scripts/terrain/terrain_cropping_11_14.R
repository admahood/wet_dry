library("raster")
library("dplyr")
system("aws s3 sync s3://earthlab-amahood/data/terrain_gb data/terrain")
dir.create("data/mucc_ter")
ter_files <- list.files("data/terrain", full.names = T)
ter_names <- list.files("data/terrain", full.names = F)

s3_path <- "s3://earthlab-amahood/data/ls5_mucc_2011"
local_path <- "data/ls5_mucc1"

system(paste0("aws s3 sync ", s3_path, " ", local_path))

ls5_files <- list.files(local_path, full.names = T)

ls5 <-raster(ls5_files)
t1 <- raster(ter_files[1])

ne <- projectExtent(ls5, crs=crs(t1))
res(ne) <- 30

for (i in 1:length(ter_files)){
  print(ter_names[i])
  t2 <- raster(ter_files[i])
    # crop(ne)%>%
  t2p <- projectRaster(t2, crs = crs(ls5)) 
    # resample(ls5)
    writeRaster(t2, filename = paste0("data/mucc_ter/", ter_names[i]), overwrite=T)
  system(paste0("aws s3 cp data/mucc_ter/",ter_names[i], 
                " s3://earthlab-amahood/data/terrain_gb/", "full_", ter_names[i]))
  gc()
}
