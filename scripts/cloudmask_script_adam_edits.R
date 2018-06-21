####6/19 code starts here. basic mask creation and application to single scene accomplished 
#as well as creation of stacks from each scene for figuring out how to replace masked values ####

# setup ------------------------------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
# turn off factors
options(stringsAsFactors = FALSE)

#list tar filenames in landsat_cloudmask_test 
# changed to Sys.glob so there weren't also directories in there
# added in stuff to pick out path-row

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
      
      x <- crop(masked[[2]], masked[[1]])
      final <- crop(masked[[1]], x)
      compareRaster(final, x)
      
      
      #brackets from beginning      
    }
  }
  system("rm -r data/ls5/")
}
#create generic mask creation function for use in overlay (can be done with basic raster arithmetic but overlay is easier on memory)
# functions should go in the beginning of the script( or in a separate functions file)
maskcreate<- function(x, y){
  x[y != 66] <- NA
  return(x)
}



#check to see if masked rasters have same extent

compareRaster(june4_masked, july6_masked)

#extents do not match exactly. resolve this with resample

july6_masked <- resample(july6_masked, june4_masked)
#july6_masked <- crop(july6_masked, june4_masked)


#again, check to ensure resample matched the extents

compareRaster(june4_masked, july6_masked)

#replace masked values in one raster with values from another

july6_replaced <- cover(july6_masked, june4_masked)

plotRGB(july6_replaced, stretch = "lin")
plot(july6_replaced)
