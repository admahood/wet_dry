library(dplyr)

dd <- "/home/a/data/vegbank"
files <- list.files(data_dir)

oc <- read.csv(file.path(dd,files[1])) %>% as_tibble()
pc <- read.csv(file.path(dd,files[2])) %>% as_tibble()
pt <- read.csv(file.path(dd,files[4])) %>% as_tibble()
ste <- read.csv(file.path(dd,files[5])) %>% as_tibble()
str <- read.csv(file.path(dd,files[6])) %>% as_tibble()


pe <- read.csv(file.path(dd,files[3])) %>% 
  as_tibble() %>%
  dplyr::select(observation_id,
                latitude,
                longitude,
                percentrockgravel,
                percentbaresoil,
                treecover,
                shrubcover,
                fieldcover,
                slopeaspect,
                slopegradient,
                elevation
                )
  