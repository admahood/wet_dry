####6/19 code starts here. basic mask creation and application to single scene accomplished 
#as well as creation of stacks from each scene for figuring out how to replace masked values ####

# setup ------------------------------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
# turn off factors
options(stringsAsFactors = FALSE)

#create generic mask creation function for use in overlay (can be done with basic raster arithmetic but overlay is easier on memory)
# functions should go in the beginning of the script( or in a separate functions file)
maskcreate<- function(x, y){
  x[y != 66] <- NA
  return(x)
}

# big loop ---------------------------------------------------------------------

years <- 1984:2011
year = years[1] # placeholder before loop
dir.create("data/results")
dir.create("data/needs")

for(year in years){
  system(paste0("aws s3 sync s3://earthlab-amahood/data/landsat/landsat_", year,
                " data/ls5/"))
  prcs <- list.files("data/ls5") %>% substr(5,10) %>% table() %>% as.data.frame()
  colnames(prcs) <- c("prc", "freq")
  prcs$prc <- as.character(prcs$prc)
  prcs$year <- year
  prcs$file = NA
  for(i in 1:nrow(prcs)){
    prcs[i, 4] <- Sys.glob(paste0("data/ls5/*", prcs$prc[i], "*"))[1]
  }
  needs <- prcs[prcs$freq == 1,]
  write.csv(needs, paste0("data/needs/needs_",year,".csv"))
  
  foreach(path_row_combo = prcs$prc) %dopar% {
    dir.create("data/scrap")
    if(prcs[prcs$prc == path_row_combo,]$freq > 1){
      t0 <- Sys.time()
      filenamef <- paste("ls5", year, path_row_combo,".tif", sep = "_")
      
      if(!file.exists(file.path("data/results/", filenamef))){
        tar_path <- "/home/rstudio/wet_dry/data/ls5/"
        tar_list <- Sys.glob(paste0(tar_path, "LT05", path_row_combo,"*.gz"))
        
        qas <- data.frame(filenames = NA, value66 = NA, goodpix = NA, i = NA)
        for (i in 1:length(tar_list)) {
          exdir <- paste0("data/scrap/",i)
          dir.create(exdir)
          untar(tar_list[i], exdir = exdir)
          qas[i, 1] <- Sys.glob(paste0(exdir, "/*pixel_qa.tif"))
          x<-freq(raster(Sys.glob(paste0(exdir, "/*pixel_qa.tif")))) %>% as_tibble()
          qas[i, 2] <- x[1,1]
          qas[i, 3] <- x[1,2]
          qas[i, 4] <- i
        }
        qas <- dplyr::arrange(qas, desc(goodpix))
        ordered_i <- qas$i
        
        tifs <- list()
        for(i in 1:length(tar_list)){
          tifs[[i]] <- Sys.glob(paste0("data/scrap/", i, "/*band*.tif"))
        }
        
        bands <- list()
        qa <- list()
        masked <- list()
        xmins <- c()
        xmaxs <- c()
        ymins <- c()
        ymaxs <- c()
        for(i in 1:length(tar_list)){
          bands[[i]] <- stack(tifs[[ordered_i[i][1]]])
          qa[[i]] <- raster(Sys.glob(paste0("data/scrap/",ordered_i[i][1],"/*pixel_qa.tif")))
          masked[[i]] <- overlay(x <- bands[[i]], y = qa[[i]], fun = maskcreate) # this takes time
          e <- extent(bands[[i]])
          xmins[i] <- e@xmin
          xmaxs[i] <- e@xmax
          ymins[i] <- e@ymin
          ymaxs[i] <- e@ymax
        }
        
        e <- extent(bands[[1]])
        
        e@xmin <- max(xmins)
        e@xmax <- min(xmaxs)
        e@ymin <- max(ymins)
        e@ymax <- min(ymaxs)
        
        masked <- lapply(masked, FUN = raster::crop, y = e) # applies crop to a list of rasters
        
        print(compareRaster(masked[[1]], masked[[2]]))
        
        final <- cover(masked[[1]], masked[[2]])
        
        if(length(tar_list) == 3){
          final <- cover(final, masked[[3]])
        }else{print('yay')}
        
        names(final) <- c("band_1", "band_2", "band_3", "band_4", "band_5", "band_7")
        
        writeRaster(final, paste0("data/results/",filenamef), overwrite = TRUE)
        system(paste0("aws s3 cp",
                      " data/results/", filenamef, 
                      " s3://earthlab-amahood/data/landsat_pixel_replaced/", filenamef))
        gc()
        print(Sys.time() - t0)
      }else{print("skipping")}
    }else{print("not enough")}
    system("rm -r data/scrap/")
  }
  system("rm -r data/ls5/")
}
