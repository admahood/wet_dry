# replacing clouds in one landsat scene with pixels from another landsat scene
# parallelized
# authors: dylan murphy and adam mahood
#
# this script requires over 10GB of ram per core!

# setup ------------------------------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(foreach)
library(doParallel)


# create generic mask creation function for use in overlay (can be done with basic raster arithmetic but overlay is easier on memory)
# functions should go in the beginning of the script( or in a separate functions file)
maskcreate <- function(x, y){
  x[y != 66] <- NA
  return(x)
}

# big loop ---------------------------------------------------------------------

years <- 1984:2011 # parallel iterator
dir.create("data")
dir.create("data/results")
dir.create("data/needs")
dir.create("data/scrap")

# this is grabbing the list of already done files from s3, and intern = TRUE saves the output as a
# vector, then i'm just trimming the fat and making a nice vector to work with
done <- system("aws s3 ls s3://earthlab-amahood/data/landsat_pixel_replaced/", intern = TRUE) %>%
  strsplit("_")
done <- done[-1]
for(i in 1:length(done)){
  done[[i]] <- done[[i]][2:3]
  done[[i]] <- paste(done[[i]], collapse = "_")
}
done <- unlist(done)


# parallel prep
corz <- length(years) # using only 28 cores out of 64 possible due to memory (RAM) needs
registerDoParallel(corz)

foreach(year = years) %dopar% { # note that foreach has a slightly different syntax
  
  system(paste0("aws s3 sync s3://earthlab-amahood/data/landsat/landsat_", year,
                " data/ls5/y",year,
                " --only-show-errors"))
  
  # creating a table of path row combinations and how many files of each we have
  prcs <- list.files(paste0("data/ls5/y",year,"/")) %>% substr(5,10) %>% table() %>% as.data.frame()
  colnames(prcs) <- c("prc", "freq")
  prcs$prc <- as.character(prcs$prc)
  
  # adding some extra info to write out so we can download more scenes
  prcs$year <- year
  prcs$file = NA
  for(i in 1:nrow(prcs)){
    prcs[i, 4] <- Sys.glob(paste0("data/ls5/y", year,"/*", prcs$prc[i], "*"))[1]
  }
  needs <- prcs[prcs$freq == 1,]
  write.csv(needs, paste0("data/needs/needs_",year,".csv"))
  system(paste0("aws s3 cp",
                " data/needs/needs_", year, ".csv",
                " s3://earthlab-amahood/data/needs/needs_", year, ".csv",
                " --only-show-errors"))
  
  # using the prc column out of that data frame as our iterator
  
  for(path_row_combo in prcs$prc){
    
    #creates unique filepath for temp directory for each parallel worker
    #very important for not running out of space
    tmpd<- paste0("data/tmp",year, "_", path_row_combo)
    dir.create(tmpd)
    #sets temp directory
    rasterOptions(tmpdir=tmpd)
    
    # so we done repeat what we've already finished
    inq <- paste(year, path_row_combo, sep = "_")
    system(paste("echo", year, path_row_combo, "already done =",any(inq==done)))
    if(!any(done == inq)){
    # this if statement ensures that we're only going through this whole thing
    # for path/row combos where we have more than one scene
    
    if(prcs[prcs$prc == path_row_combo,]$freq > 1){
      
      # this is the result filename, to be used at the end. defnining it here
      # so that we can avoid creating it a second time if it's already made
      # i.e. that's the upcoming if statement
      filenamef <- paste("ls5", year, path_row_combo,".tif", sep = "_")
      
      # this is the way to have the script print something out from a parallel script
      # the print() function doesn't work in parallel
      
      
      if(!file.exists(file.path("data/results/", filenamef))){
        
        # the a directory where the compressed landsat files are coming from
        tar_path <- paste0("/home/rstudio/wet_dry/data/ls5/y",year,"/")
        tar_list <- Sys.glob(paste0(tar_path, "LT05", path_row_combo,"*.gz"))
        
        qas <- data.frame(filenames = NA, value66 = NA, goodpix = NA, i = NA)
        
        # deleting everything from previous loops
        system(paste0("rm -r data/scrap/",year, "/*"))
        for (i in 1:length(tar_list)) {
          exdir <- paste0("data/scrap/",year, "/",i)
          dir.create(exdir)
          untar(tar_list[i], exdir = exdir)
          qas[i, 1] <- Sys.glob(paste0(exdir, "/*pixel_qa.tif"))
          x<-freq(raster(Sys.glob(paste0(exdir, "/*pixel_qa.tif")))) %>% as_tibble()
          qas[i, 2] <- x[1,1]
          qas[i, 3] <- x[1,2]
          qas[i, 4] <- i
        }
        rm(x)
        qas <- dplyr::arrange(qas, desc(goodpix))
        ordered_i <- qas$i
        
        system(paste("echo masks created", year, path_row_combo))
        
        # creating lists of the tifs only with band in the name for each scene
        tifs <- list()
        for(i in 1:length(tar_list)){
          tifs[[i]] <- Sys.glob(paste0("data/scrap/",year,"/", i, "/*band*.tif"))
        }
        
        bands <- list() # each item of this list will have bands 1-5 & 7
        qa <- list() # each item of this list will have the qa band
        masked <- list() # each item of this list will be the masked result
        
        xmins <- c() # each of these will have the pieces of the extent objects
        xmaxs <- c() # to be used to create the extent object that works for all 
        ymins <- c()
        ymaxs <- c()
        for(i in 1:length(tar_list)){
          bands[[i]] <- stack(tifs[[ordered_i[i][1]]])
          qa[[i]] <- raster(Sys.glob(paste0("data/scrap/",year, "/", ordered_i[i][1],"/*pixel_qa.tif")))
          masked[[i]] <- overlay(x <- bands[[i]], y = qa[[i]], fun = maskcreate) # this takes time
          e <- extent(bands[[i]])
          xmins[i] <- e@xmin
          xmaxs[i] <- e@xmax
          ymins[i] <- e@ymin
          ymaxs[i] <- e@ymax
        }
        rm(qa)
        e <- extent(bands[[1]])
        
        e@xmin <- max(xmins) # creating the one exent object
        e@xmax <- min(xmaxs)
        e@ymin <- max(ymins)
        e@ymax <- min(ymaxs)
        
        rm(bands)
        
        # cropping the masked rasters to the one exent using lapply (since everything is in lists)
        masked <- lapply(masked, FUN = raster::crop, y = e) # applies crop to a list of rasters
        
        system(paste("echo mask applied", year, path_row_combo))
        
        # doing the pixel replacement, then also going at it with the third mask, if it exists
        final <- cover(masked[[1]], masked[[2]])
        
        if(length(tar_list) == 3){
          final <- cover(final, masked[[3]])
        }else{print('yay')}
        rm(masked)
        
        # renaming the bands
        names(final) <- c("band_1", "band_2", "band_3", "band_4", "band_5", "band_7")
        
        #writing everything out an deleting (this script gobbles up an insane amount of space)
        writeRaster(final, paste0("data/results/",filenamef), overwrite = TRUE)
        rm(final)
        system(paste0("aws s3 cp",
                      " data/results/", filenamef, 
                      " s3://earthlab-amahood/data/landsat_pixel_replaced/", filenamef))
        system(paste0("rm data/results/", filenamef))
        for(i in 1:length(tar_list)){
          system(paste0("rm ", tar_list[i]))
        }
        gc()
        system(paste0("rm -r ", tmpd)) #removing the temporary files
        
      }else{system("echo skipping")}
    }else{system("echo not enough")}
    }
  }
}
