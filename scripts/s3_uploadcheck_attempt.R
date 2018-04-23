my_bucket <- get_bucket("earthlab-amahood")
landsat_2011_files <- get_bucket(my_bucket, prefix = "data/landsat7/landsat_2011")
landsat_2012_files <- get_bucket(my_bucket, prefix = "data/landsat7/landsat_2012")
landsat_2013_files <- get_bucket(my_bucket, prefix = "data/landsat7/landsat_2013")
landsat_2014_files <- get_bucket(my_bucket, prefix = "data/landsat7/landsat_2014")
landsat_2015_files <- get_bucket(my_bucket, prefix = "data/landsat7/landsat_2015")






list_2011 <- list()
list_2011 <- for(i in length(landsat_2011_files), {
  list(i) <- unlist(landsat_2011_files)
}
list_2012 <- lapply(landsat_2012_files, get_object)
list_2013 <- lapply(landsat_2013_files, get_object)
list_2014 <- lapply(landsat_2014_files, get_object)
list_2015 <- lapply(landsat_2015_files, get_object)
summary(landsat_2011_files)
