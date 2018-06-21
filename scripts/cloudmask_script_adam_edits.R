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
  
  
  for(path_row_combo in prcs$prc){
    if(prcs[prcs$prc == path_row_combo,]$freq > 1){
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
      for(i in 1:length(tar_list)){
        bands[[i]] <- stack(tifs[[ordered_i[i][1]]])
        qa[[i]] <- raster(Sys.glob(paste0("data/scrap/",ordered_i[i][1],"/*pixel_qa.tif")))
        masked[[i]] <- overlay(x <- bands[[i]], y = qa[[i]], fun = maskcreate) # this takes time
      }
      e <- extent(bands[[1]])
      f <- extent(bands[[2]])
      
      e@xmin <- max(c(e@xmin, f@xmin))
      e@xmax <- min(c(e@xmax, f@xmax))
      e@ymin <- max(c(e@ymin, f@ymin))
      e@ymax <- min(c(e@ymax, f@ymax))
      
      masked <- lapply(masked, FUN = raster::crop, y = e)
      
      print(compareRaster(masked[[1]], masked[[2]]))
      
      final <- cover(masked[[1]], masked[[2]])

      #brackets from beginning      
    }
  }
  system("rm -r data/ls5/")
}



july6_replaced <- cover(july6_masked, june4_masked)

plotRGB(masked[[1]], stretch = "lin")
plotRGB(final, stretch = "lin")
