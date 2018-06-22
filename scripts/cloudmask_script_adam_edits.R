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
path_row_combo <- prcs$prc[1] # placeholder before loop
year = years[1] # placeholder before loop
dir.create("data/scrap")

for(year in years){
  system(paste0("aws s3 sync s3://earthlab-amahood/data/landsat/landsat_", year,
                " data/ls5/"))
  prcs <- list.files("data/ls5") %>% substr(5,10) %>% table() %>% as.data.frame()
  colnames(prcs) <- c("prc", "freq")
  prcs$prc <- as.character(prcs$prc)
  
  t0 <- Sys.time()
  for(path_row_combo in prcs$prc){
    if(prcs[prcs$prc == path_row_combo,]$freq > 1){
      filenamef <- paste("ls5", year, path_row_combo,".tif", sep = "_")
      
      if(!file.exists(file.path("data/scrap/", filenamef))){
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
        
        writeRaster(final, paste0("data/scrap/",filenamef), overwrite = TRUE)
        system(paste0("aws s3 cp",
                      " data/scrap/", filenamef, 
                      " s3://earthlab-amahood/data/landsat_pixel_replaced/", filenamef))
        gc()
      }else{print("skipping")}
    }else{write.csv(paste("already have", Sys.glob(paste0("data/ls5/*",path_row_combo,"*"))),
                    paste0("data/scrap/we_need_more", filenamef, ".csv"))
      system(paste0("aws s3 cp ", paste0("data/scrap/we_need_more", filenamef, ".csv"),
                    " s3://earthlab-amahood/data/landsat_pixel_replaced/needmore/",
                    paste0("we_need_more", filenamef, ".csv")))
    }
    system("rm -r data/scrap/")
    print(Sys.time() - t0)
  }
  system("rm -r data/ls5/")
}
