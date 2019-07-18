# script to input analysis ready landsat data, make mean composite

ls_ard_path <- "/home/a/data/landsat_ard"
scrap_path <- "scrap"
dir.create(scrap_path)

tar_files <- list.files(ls_ard_path, pattern = ".tar", full.names = TRUE)

for(i in tar_files){
  untar(i, exdir = path.expand(scrap_path))
  
}

