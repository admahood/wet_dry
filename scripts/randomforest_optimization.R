# setup ------------------------------------------------------------------------
libs <- c("randomForest", "tidyverse","sf", "foreach", "doParallel",
          "caTools", # for sample.split()
          "caret", # for confusionMatrix
          "ranger",
          "spdep", #for the weights
          "rfUtilities" # for multi.collinear()
          )

#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

source("scripts/functions.R")

date <- paste(strsplit(date()," ")[[1]][c(2,3,5)],collapse="_")
set.seed(11)

corz <- detectCores()-1

# Load and manipulate Data -----------------------------------------------------

system("aws s3 cp s3://earthlab-amahood/data/plots_with_landsat.gpkg data/plot_data/plots_with_landsat.gpkg")
system("aws s3 cp s3://earthlab-amahood/data/vegbank_plots_with_landsat.gpkg data/plot_data/vegbank_plots_with_landsat.gpkg")

# blm-aim data 

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(sr_band3, sr_band5),
         folded_aspect_ns = get_folded_aspect_ns(aspect)) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                ndvi=NDVI, evi=EVI, savi=SAVI,sr=SR, ndsvi,
                greenness, brightness, wetness, OBJECTID,Latitude,
                total_shrubs,
                BareSoilCo,InvAnnGras, InvAnnFo_1,InvPlantCo,
                GapPct_25_,GapPct_51_, GapPct_101, GapPct_200, GapPct_251,
                TotalFolia, SagebrushC,
                elevation,
                folded_aspect_ns,
                slope, tpi=TPI, tri=TRI, roughness, flowdir) %>%
  mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7),
         tndvi = (ndvi+1)*50,
         dup = duplicated(Latitude)) %>%
  filter(dup == F) %>%
  dplyr::select(-dup, -OBJECTID)


# splitting the data into response and predictor variables -------------------------------------------------------------

resp <- dplyr::select(gbd, SagebrushC,
                      TotalFolia,
                      BareSoilCo,
                      #InvAnnGras, 
                      #InvAnnFo_1,
                      InvPlantCo
                      ) %>%
  st_set_geometry(NULL) 
clm <- dplyr::select(gbd, sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                     elevation,ndvi,evi,satvi,tndvi,sr,ndsvi,Latitude,
                     folded_aspect_ns, brightness, greenness, wetness,
                     slope, tpi, tri, roughness, flowdir) %>%
  st_set_geometry(NULL) %>%
  mutate(index57 = (sr_band5 - sr_band7)/(sr_band5+sr_band7),
         green_ndvi = (sr_band4 - sr_band2)/(sr_band4+sr_band2),
         moisture_index = (sr_band4 - sr_band5)/(sr_band4+sr_band5),
         SLA_index = sr_band4/(sr_band3+sr_band7))

# running loops of single and multivariate models to check out relationships---------
# (easier than plotting) 
res_df <- data.frame(resp = NA, pred1 = NA, pred2=NA,R2 = NA, p = NA, est = NA)
nb_df <- data.frame(pred = NA, rsq=NA,aic=NA, p=NA, est=NA)
counter <- 1
#univariate
d <- mutate(cbind(resp,clm), SagebrushC = SagebrushC/100)
for (r in 1:ncol(dplyr::select(d,-SagebrushC))){
  f <- formula(paste0("SagebrushC ~ ",  names(dplyr::select(d,-SagebrushC))[r]))
  x <- summary(glm(f, d, family=quasibinomial(link="logit")))
  nb_df[counter, 1] <- names(dplyr::select(d,-SagebrushC))[r]
  nb_df[counter, 2] <- round(rsq::rsq(glm(f, d, family="binomial")),4)
  nb_df[counter, 3] <- x$aic
  nb_df[counter, 4] <- round(x$coefficients[2,4], 4)
  nb_df[counter, 5] <- round(x$coefficients[2,1], 4)
  counter = counter +1
}
arrange(nb_df,desc(rsq))[1:20,]

rsp <- resp/100
dd <- cbind(rsp,clm)


counter <- 1
for(r in 1:ncol(resp)){
  for(d in 1:ncol((clm))){
      f <- formula(paste0(names(resp)[r], "~",  names(clm)[d]))
      m <- glm(f, data = dd, family=quasibinomial(link="logit"))
      x <- summary(m)
      res_df[counter, 1] <- names(resp)[r]
      res_df[counter, 2] <- names((clm))[d]
      res_df[counter, 3] <- NA
      res_df[counter, 4] <- round(rsq::rsq(m),4)
      res_df[counter, 5] <- round(x$coefficients[2,4], 4)
      res_df[counter, 6] <- round(x$coefficients[2,1], 4)
      counter = counter +1
    
  }
}

#bivariate
for(r in 1:ncol(resp)){
  for(c in 1:ncol(clm)){
    for(e in 1:ncol(clm)){
      
      f <- formula(paste0(names(resp)[r], "~", names(clm)[c],
                          "+", names((clm))[e]))
      m <- glm(f, data = dd, family=quasibinomial(link="logit"))
      x <- summary(m)
      res_df[counter, 1] <- names(resp)[r]
      res_df[counter, 2] <- names(clm)[c]
      res_df[counter, 3] <- names((clm))[e]
      res_df[counter, 4] <- round(rsq::rsq(m),4)
      res_df[counter, 5] <- round(x$coefficients[2,4], 4)
      res_df[counter, 6] <- round(x$coefficients[2,1], 4)
      counter = counter +1
    }
  }
}
# total veg cover (TotalFolia) and bare soil (BareSoilCo) are best, and elevation 
# is not a good predictor. I found that bands 5 and 7 were always good, so I made
# an NDVI-type index of them (index57) and that worked well
# but invasive annual grass and invasive annual grasses and forbs combined also
# does pretty good
# Sagebrush cover was not easily predicted. Perhaps we can use this info to develop
# a way to group sites based on these response variables that are better able 
# to be predicted as our class labels.
# also, i discovered that our total shrubs variable is sometimes over 100...
# that's not good lol
arrange(res_df[res_df$resp == "InvAnnGras",],desc(R2))[1:20,]
arrange(res_df[res_df$resp == "InvAnnFo_1",],desc(R2))[1:20,]
arrange(res_df[res_df$resp == "SagebrushC",],desc(R2))[1:20,]
arrange(res_df[res_df$resp == "BareSoilCo",],desc(R2))[1:20,]

arrange(res_df,desc(R2))[1:20,]

# now plotting!! ---------------------------------------------------------------

m <- glm(SagebrushC~InvPlantCo + elevation +green_ndvi, dd, family=quasibinomial(link="logit"))
summary(m); rsq::rsq(m) # still just .266
ggplot(data=dd,aes(y=SagebrushC,x=green_ndvi))+
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family=quasibinomial(link="logit")), 
              se = TRUE) +
  theme_bw()

m <- glm(TotalFolia ~ index57+ sr_band2*green_ndvi, dd, family=quasibinomial(link="logit"))
summary(m); rsq::rsq(m)
ggplot(data=dd,aes(y=TotalFolia,x=index57))+
         geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family=quasibinomial(link="logit")), 
              se = TRUE) +
  theme_bw()

m <- glm(BareSoilCo ~ index57+ sr_band4*brightness, dd, family=quasibinomial(link="logit"))
summary(m); rsq::rsq(m)
ggplot(data=dd,aes(y=BareSoilCo,x=index57))+
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family=quasibinomial(link="logit")), 
              se = TRUE) +
  theme_bw()

x<-prcomp(resp)$x %>%as.data.frame
biplot(prcomp(resp))
dd1 <- cbind(dd, x)
ggplot(dd1, aes(size=SagebrushC, color=TotalFolia, y=PC1, x=PC2))+
  geom_point()
ggplot(dd1, aes(size=InvAnnFo_1, color=BareSoilCo, y=PC1, x=PC2))+
  geom_point()

# unsupervised clustering using random forest  ----------------------------------
# any of the clustering methods will probably work and the veg clustering 
# methods out of the vegan package might be better, but just checking this out.
# looks like it works pretty good (see teh MDS plot)

# why not just manually attempt to get rid of mixed pixels? --------------------
shrubs<- dplyr::filter(gbd, SagebrushC > 0 & InvAnnGras < 2) %>%
  mutate(cluster = "shrub")
grasses <- dplyr::filter(gbd, SagebrushC < 2 & InvAnnGras >2)%>%
  mutate(cluster="grass")

gbd_new <- rbind(grasses,shrubs) %>%
  dplyr::select(cluster, sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                elevation,ndvi,evi,satvi,tndvi,sr,ndsvi,Latitude,
                folded_aspect_ns, brightness, greenness, wetness,
                slope, tpi, tri, roughness, flowdir) %>%
  mutate(index57 = (sr_band5 - sr_band7)/(sr_band5+sr_band7),
         green_ndvi = (sr_band4 - sr_band2)/(sr_band4+sr_band2),
         moisture_index = (sr_band4 - sr_band5)/(sr_band4+sr_band5),
         SLA_index = sr_band4/(sr_band3+sr_band7))

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
coords_sf <- st_transform(gbd_new, albers) %>%
  st_coordinates() 
nb_sf <- spdep::knn2nb(spdep::knearneigh(coords_sf, k=4))
dsts <- spdep::nbdists(nb_sf, coords_sf)

www <- vector()
for(i in 1:nrow(gbd_new)){
  weightsum <- dsts[[i]] %>% sum()
  weight <- ((weightsum/4))
  www[i]<- weight
}



rf_us <- randomForest(resp, ntree=3000, proximity = TRUE)
library(cluster)
hclust(vegan::vegdist(resp, method="bray"), method="average") -> clust

# note here that k is the number of clusters
resp$cluster <- as.factor(pam(1-rf_us$proximity, k=2, diss = TRUE)$clustering)

# looks like 2 works good in this case
MDSplot(rf_us, resp$cluster)
table(resp$cluster)
ggplot(resp, aes(y=SagebrushC, x=TotalFolia, color = cluster))+
  geom_point()

# now, we use whatever classification label
ddd<- list()
#ddd[[1]] <- cbind(clm,dplyr::select(resp,cluster)) 
ddd[[1]] <- gbd_new
rvars <- ncol(ddd[[1]])-2

# randomForest(cluster~., ddd)

control <- trainControl(method='repeatedcv',
                        number=10, 
                        repeats=3,
                        search="grid")
var_results <- data.frame(accuracy=NA, nvars = NA, dropped = NA)
for (i in 1:rvars){ # this takes 10-30 minutes
  
  # albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  # coords_sf <- st_transform(ddd[[i]], albers) %>%
  #   st_coordinates() 
  # nb_sf <- spdep::knn2nb(spdep::knearneigh(coords_sf, k=4))
  # dsts <- spdep::nbdists(nb_sf, coords_sf)
  # 
  # ddd[[i]]$www <- NA
  # for(j in 1:nrow(ddd[[i]])){
  #   weightsum <- dsts[[j]] %>% sum()
  #   weight <- ((weightsum/4))
  #   ddd[[i]]$www[j]<- weight
  # }

  tgrid <- expand.grid(
    .mtry = 1:round(sqrt(ncol(ddd[[i]])-2)), #cluster and geom don't count
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  # next time this is ran, do rf[[i]] to be able to look at the models later
  rf <- train(cluster~., 
                      data=st_set_geometry(ddd[[i]], NULL), 
                      method='ranger',
                      metric=c('Accuracy'), # or RMSE?
                      tuneGrid=tgrid, 
                      trControl=control,
                      #case.weights = www, # not sure why this doesn't work
                      importance = "permutation")
  
  var_results[i, 1] <- max(rf$results$Accuracy)
  var_results[i, 2] <- i
  
  
  least_important <- caret::varImp(rf)$importance %>%
    rownames_to_column("var") %>%
    arrange(Overall)
  vvv <- least_important[1,1]
  ddd[[i+1]] <- dplyr::select(ddd[[i]],-vvv)
  
  var_results[i, 3] <- vvv
  print(paste(i/rvars*100, "%"))
}

names(ddd[[23]]) -> nnn
nnn <- nnn[2:length(nnn)-1] #dropping cluster and geom
ggplot(var_results, aes(x=nvars, y=accuracy)) + 
  geom_line() +
  xlab("# Variables dropped (out of 27)") +
  geom_vline(xintercept = 23, lty=3) +
  annotate("text",x=0, y=0.86, hjust=0, vjust=1,
           label = paste("Remaining Variables (after dropping 22): \n",
                         nnn[1],"\n",nnn[2],"\n",nnn[3],"\n",nnn[4], "\n",
                         nnn[5]#, "\n",nnn[6], "\n",nnn[7],"\n",nnn[8], "\n",
                         #nnn[9], "\n", nnn[10],"\n",nnn[11],"\n",nnn[12])) + 
           )) +
    ggsave("var_dropping_2.pdf")

# creating weights

# gbd$lat <- st_coordinates(gbd)[,2]

# gbd_sp  <- as_Spatial(gbd) %>%
#   sp::spTransform(albers)
# dt.vgm <- gstat::variogram(total_shrubs~1, gbd_sp)
# dt.fit <- gstat::fit.variogram(dt.vgm, model = gstat::vgm("Sph")) # fit model
# plot(dt.vgm,dt.fit)



# gstat::variogramST(total_shrubs~1, 
#                    locations = gbd_sp@coords, 
#                    tlags=1:4, 
#                    data = gbd_sp@data$year)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
coords_sf <- st_transform(gbd, albers) %>%
  st_coordinates() 
nb_sf <- spdep::knn2nb(spdep::knearneigh(coords_sf, k=4))
dsts <- spdep::nbdists(nb_sf, coords_sf)

gbd$weights <- NA
for(i in 1:nrow(gbd)){
  weightsum <- dsts[[i]] %>% sum()
  weight <- ((weightsum/4))
  gbd$weights[i]<- weight
}

maxw<-max(gbd$weights)
gbd$weights <- gbd$weights/maxw


gbd<-  st_set_geometry(gbd,NULL)%>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7)) 
  

gtrain <- filter(gbd,split ==TRUE) %>% dplyr::select(-split)
write.csv(gtrain, paste0("gtrain_",date,".csv"))
system(paste0("aws s3 cp gtrain_",
              date,
              ".csv s3://earthlab-amahood/data/data_splits/gtrain_",
              date,".csv"))

#multi.collinear(gtrain)

gdevtest <- filter(gbd,split ==FALSE) %>%
  mutate(split = sample.split(split, SplitRatio=0.5))

gdev <- filter(gdevtest, split==TRUE) %>% dplyr::select(-split)
write.csv(gdev, paste0("gdev_",date,".csv"))
system(paste0("aws s3 cp gdev_",date,
              ".csv s3://earthlab-amahood/data/data_splits/gdev_",date,".csv"))

gtest <- filter(gdevtest, split==FALSE) %>% dplyr::select(-split)
write.csv(gtest, paste0("gtest_",date,".csv"))
system(paste0("aws s3 cp gtest_",date,
              ".csv s3://earthlab-amahood/data/data_splits/gtest_",date,".csv"))

#vegbank data

vbd <- st_read("data/plot_data/vegbank_plots_with_landsat.gpkg", quiet=T) %>%
  mutate(ndsvi = get_ndsvi(sr_band3, sr_band5),
         folded_aspect_ns = get_folded_aspect_ns(aspect)) %>%
  rename(total_shrubs = shrubcover, esp_mask = binary) %>%
  dplyr::select(sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                ndvi, evi, savi,sr, ndsvi,
                greenness, brightness, wetness,
                total_shrubs,
                elevation,
                folded_aspect_ns,
                slope, folded_aspect, tpi, tri, roughness, flowdir)  %>%
  mutate(satvi = get_satvi(sr_band3, sr_band5,sr_band7),
         tndvi = (ndvi+1)*50)
vbd$lat <- st_coordinates(vbd)[,2]

vbd <- mutate(vbd, dup = duplicated(lat)) %>% 
  filter(dup == F) %>%
  dplyr::select(-dup) %>%
  st_set_geometry(NULL) %>%
  mutate(split = 1,
         split = sample.split(split, SplitRatio=0.7))

vtrain <- filter(vbd,split ==TRUE) %>% dplyr::select(-split)
write.csv(vtrain, paste0("vtrain_",date,".csv"))
system(paste0("aws s3 cp vtrain_",
              date,
              ".csv s3://earthlab-amahood/data/data_splits/vtrain_",
              date,".csv"))

vdevtest <- filter(vbd,split ==FALSE) %>%
  mutate(split = sample.split(split, SplitRatio=0.5))

vdev <- filter(vdevtest, split==TRUE) %>% dplyr::select(-split)
write.csv(vdev, paste0("vdev_",date,".csv"))
system(paste0("aws s3 cp vdev_",date,
              ".csv s3://earthlab-amahood/data/data_splits/vdev_",date,".csv"))

vtest <- filter(vdevtest, split==FALSE) %>% dplyr::select(-split)
write.csv(vtest, paste0("vtest_",date,".csv"))
system(paste0("aws s3 cp vtest_",date,
              ".csv s3://earthlab-amahood/data/data_splits/vtest_",date,".csv"))

# creating the hypermatrix -----------------------------------------------------
mtry <- seq(1,8,1) # 22 = # cols in the yet to be created training set
sc <- seq(4,25,1)
# nodesize <- seq(1,4,1)

hyper_grid <- expand.grid(mtry = mtry, 
                          sc=sc) ; nrow(hyper_grid)

# running the hypermatrix in parallel ------------------------------------------

registerDoParallel(corz)

hr <- foreach (i = 1:nrow(hyper_grid), .combine = rbind) %dopar% {
  
    train <- mutate(gbd,
                  binary = as.factor(
                    ifelse(
                      total_shrubs < hyper_grid$sc[i], "Grass", "Shrub"))) %>%
      dplyr::select(-total_shrubs) %>%
      st_set_geometry(NULL)
  
  if(nrow(train[train$binary == "Grass",])<nrow(train[train$binary == "Shrub",])){
    gps <- train[train$binary == "Grass",]
    sps <- train[train$binary == "Shrub",]
    nsps <- sample_n(sps, nrow(gps))
    train <- rbind(gps,nsps)
  }else{
    gps <- train[train$binary == "Grass",]
    sps <- train[train$binary == "Shrub",]
    ngps <- sample_n(gps, nrow(sps))
    train <- rbind(sps,ngps)
  }
    

  # Train a Random Forest model
  m <- ranger(formula = binary ~ ., 
                              data = dplyr::select(train,-weights),
                              mtry = hyper_grid$mtry[i],
                              case.weights = train$weights,
                              importance = "impurity",
                              num.trees = 1000)
  #validate 
    dev1  <- mutate(vbd,
                  binary = as.factor(
                    ifelse(
                      total_shrubs < hyper_grid$sc[i], "Grass", "Shrub")))%>%
      dplyr::select(-total_shrubs) %>%
      st_set_geometry(NULL)
  
  if(nrow(dev1[dev1$binary == "Grass",])<nrow(dev1[dev1$binary == "Shrub",])){
    gps <- dev1[dev1$binary == "Grass",]
    sps <- dev1[dev1$binary == "Shrub",]
    nsps <- sample_n(sps, nrow(gps))
    dev1 <- rbind(gps,nsps)
  }else{
    gps <- dev1[dev1$binary == "Grass",]
    sps <- dev1[dev1$binary == "Shrub",]
    ngps <- sample_n(gps, nrow(sps))
    dev1 <- rbind(sps,ngps)
  }
  
  class_prediction <- predict(object = m,   # model object
                              data = dev1,  # test dataset
                              type = "response")
  
  cm <- confusionMatrix(data = class_prediction$predictions,       # predicted classes
                        reference = dev1$binary)  # actual classes
    # Store OOB error for the model                      
  imps<- importance(m) %>% 
    as.data.frame() %>% 
    rownames_to_column("var") %>% 
    arrange(desc(.)) %>%
    dplyr::select("var")
  
  w<- data.frame(
    mtry = hyper_grid$mtry[i], 
    sc=hyper_grid$sc[i],
    sensitivity = as.numeric(cm$byClass[1]),
    specificity = as.numeric(cm$byClass[2]),
    oob = m$prediction.error,
    accuracy = as.numeric(cm$overall[1]),
    kappa = as.numeric(cm$overall[2]),
    ac_lower = as.numeric(cm$overall[3]),
    ac_upper = as.numeric(cm$overall[4]),
    ac_null = as.numeric(cm$overall[5]),
    accuracy_p = as.numeric(cm$overall[6]),
    mcnemar_p = as.numeric(cm$overall[7]),
    pos_pred_value = as.numeric(cm$byClass[3]),
    neg_pred_value = as.numeric(cm$byClass[4]),
    precision = as.numeric(cm$byClass[5]),
    recall = as.numeric(cm$byClass[6]),
    F1 = as.numeric(cm$byClass[7]),
    prevalence = as.numeric(cm$byClass[8]),
    detection_rate = as.numeric(cm$byClass[9]),
    detection_prevalence = as.numeric(cm$byClass[10]),
    balanced_accuracy = as.numeric(cm$byClass[11]),
    sample_size = m$num.samples,
    var1=imps$var[1],
    var2=imps$var[2],
    var3=imps$var[3],
    var4=imps$var[4],
    var5=imps$var[5],
    var6=imps$var[6],
    var7=imps$var[7],
    var8=imps$var[8]
    
  )
  gc()
  system(paste("echo", 
               m$num.samples,
               round(as.numeric(cm$byClass[11]),2), 
               paste(round(i/nrow(hyper_grid)*100,2),"%")))
  return(w)
}

write.csv(hr, paste0("data/hg",date,".csv"))
system(paste0("aws s3 cp data/hg",date,".csv s3://earthlab-amahood/data/hypergrids_vb/hg", date,".csv"))

# below here is messing around, above is things that are used ------------------

# fitting the optimum model ----------------------------------------------------
read.csv(paste0("data/hg",date,".csv"))%>%
  arrange(desc(accuracy)) %>%
  dplyr::select(mtry,sc, sensitivity, specificity, kappa, accuracy) %>%
  as_tibble()

table(c(
table(hr[1:10,]$var1) %>%sort(decreasing = T) %>% names(),
table(hr[1:10,]$var2) %>%sort(decreasing = T)%>% names(),
table(hr[1:10,]$var3) %>%sort(decreasing = T)%>% names(),
table(hr[1:10,]$var4) %>%sort(decreasing = T)%>% names(),
table(hr[1:10,]$var5) %>%sort(decreasing = T)%>% names(),
table(hr[1:10,]$var6) %>%sort(decreasing = T)%>% names(),
table(hr[1:10,]$var7) %>%sort(decreasing = T)%>% names())
)

gbd <- mutate(gbd,
              binary = as.factor(
                ifelse(
                  total_shrubs < 18, "Grass", "Shrub")))

variables <- dplyr::select(gbd,
                           sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                           ndvi, evi, savi,sr , ndsvi, satvi,
                           greenness, brightness, wetness,
                           elevation, tndvi, weights,
                           slope, folded_aspect_ns, tpi, tri , roughness, flowdir,
                           binary)

variables$split <- sample.split(variables$binary, SplitRatio = .8) #create new variable for splitting plot data into training and test datasets (70% training data)

train <- filter(variables, split == TRUE) %>%    #create training dataset
  dplyr::select(-split)                            #remove split variable from training data
test <- filter(variables, split == FALSE) %>%
  dplyr::select(-split,-weights)

frst <- ranger(formula = binary ~ .,
                     data = dplyr::select(train, -weights),
                     mtry = 1,
                     importance="permutation",
                     case.weights = train$weights,
                     num.trees = 2000)

class_prediction <- predict(object = frst,   # model object
                            data = test,  # test dataset
                            type = "response")
cm <- confusionMatrix(data = class_prediction$predictions,       # predicted classes
                      reference = test$binary)  # actual classes
print(cm)
paste0("Test Accuracy: ", cm$overall[1])
paste("oob accuracy")
print(1 - oob_err)

rf_auc = auc(actual = test$binary,
             predicted = class_prediction)
class_prediction <- predict(object = frst,   # model object
                            newdata = test,  # test dataset
                            type = "prob")

pred = prediction(as.numeric(class_prediction[,"Shrub"]), as.numeric(test$binary))

rocs <- performance(pred, "tpr", "fpr")
plot(rocs)
varImpPlot(frst)


# gbm: first crack--------------------------------------------------------------

gbd <- st_read("data/plot_data/plots_with_landsat.gpkg", quiet=T) %>%
  filter(esp_mask == 1) %>%
  mutate(total_shrubs = NonInvShru + SagebrushC,
         ndsvi = get_ndsvi(gbd$sr_band3, gbd$sr_band5))%>%
  st_set_geometry(NULL)

gbd <- mutate(gbd, shrub = ifelse(total_shrubs > 13, 1, 0))

variables <- dplyr::select(gbd,
                           sr_band1, sr_band2, sr_band3, sr_band4, sr_band5, sr_band7,
                           ndvi = NDVI, evi = EVI, savi = SAVI,ndsvi, #data$SATVI,
                           greenness, brightness, wetness,
                           elevation,
                           slope, folded_aspect, tpi = TPI, tri = TRI, roughness, flowdir, #data$cluster,
                           #total_shrubs)
                           shrub)

variables$split <- sample.split(variables$shrub, SplitRatio = .8) #create new variable for splitting plot data into training and test datasets (70% training data)

train <- filter(variables, split == TRUE) %>%    #create training dataset
  dplyr::select(-split)                            #remove split variable from training data
test <- filter(variables, split == FALSE) %>%
  dplyr::select(-split)

set.seed(1)
gbm_mod <- gbm(formula = shrub ~ ., 
               distribution = "bernoulli", 
               data = train,
               n.trees = 10000)

preds1 <- predict(object = gbm_mod, 
                  newdata = test,
                  n.trees = 10000)

auc(actual = test$shrub, predicted = preds1)

ntree_opt_oob <- gbm.perf(object = gbm_mod, 
                          method = "OOB", 
                          oobag.curve = TRUE)

gbm_cv <- gbm(formula = shrub ~ ., 
                       distribution = "bernoulli", 
                       data = train,
                       n.trees = 10000,
                       cv.folds = 3)

ntree_opt_cv <- gbm.perf(object = gbm_cv, 
                         method = "cv",
                         oobag.curve = T)

gbm.perf(object = gbm_mod, 
         method = "test")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_oob number of trees
preds1 <- predict(object = gbm_mod, 
                  newdata = test,
                  n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2 <- predict(object = gbm_cv, 
                  newdata = test,
                  n.trees = ntree_opt_cv)   

# Generate the test set AUCs using the two sets of preditions & compare
auc1 <- auc(actual = test$shrub, predicted = preds1)  #OOB
auc2 <- auc(actual = test$shrub, predicted = preds2)  #CV 

# hypermatrix for gbm ----------------------------------------------------------




# comparing different types of models ------------------------------------------
# Compare AUC 
print(paste0("Test set AUC (OOB): ", auc1))                         
print(paste0("Test set AUC (CV): ", auc2))

# comparing different model types

actual <- test$shrub
dt_auc <- auc(actual = actual, predicted = preds2)
bag_auc <- auc(actual = actual, predicted = preds1)
rf_auc <- auc(actual = actual, predicted = rf_preds)
gbm_auc <- auc(actual = actual, predicted = gbm_preds)

# Print results
sprintf("Decision Tree Test AUC: %.3f", dt_auc)
sprintf("Bagged Trees Test AUC: %.3f", bag_auc)
sprintf("Random Forest Test AUC: %.3f", rf_auc)
sprintf("GBM Test AUC: %.3f", gbm_auc)


# List of predictions
preds_list <- list(dt_preds, bag_preds, rf_preds, gbm_preds)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(credit_test$default), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)

# adam does not understand anything below here

#### Step 9: using test data to check model performance -----------------

pred1 <- stats::predict(forest_1, newdata = rf_test, type = "response") %>%
  as.numeric()

pred <- prediction(pred1, rf_test$binary)

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

##change second argument to get different performance metrics ie false positive rate accuracy etc.##
# performance1 <- performance(prediction1, "err")


# SCRAP -----------------------------------------------------------------


#### Step 3: Set clustering variables (This part needs some work as of 6/29) -----


# gb_data <- gb_data[rowSums(dplyr::select(gb_data, NonInvShru, 
#                                          InvAnnGras,
#                                          InvAnnFo_1,
#                                          NonInvPere,
#                                          SagebrushC,
#                                          BareSoilCo
# )) > 0,]
# gb_data <- na.omit(gb_data)


# WORK ON THIS LATER FOR NARROWING DOWN BEST CLUSTERING VARIABLES (6/29)
# gb_data_clst <- dplyr::select(gb_data@data, NonInvShru, 
#InvAnnGras
#InvAnnFo_1
#NonInvPere,
# BareSoilCo)


#### remove missing values for invanngrass and sagebrushcover -- necessary for making clusters
#gb_data$InvTreeCov <- as.numeric(gb_data$InvTreeCov)
#gb_data <- gb_data[rowSums(gb_data@data[,c(31, 38)]) > 0,]
#gb_data_clst <- gb_data[,c(31, 38)]


# #### step 4 Finding Clusters for Veg. cover ####
# gbd_clst <- dplyr::select(gbd,
#                           BareSoilCo,
#                           InvAnnGras,
#                           InvAnnFo_1,
#                           SagebrushC,
#                           SagebrushH) %>%
#   st_set_geometry(NULL)
# 
# 
# #gbd_clst <- wisconsin(gbd_clst)
# 
# comm.bc.dist <- vegdist(gbd_clst, method = "bray")
# 
# comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# 
# plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", labels = FALSE)
# 
# # add cluster variable to gb_data
# gbd$cluster <- as.factor(cutree(comm.bc.clust, 3))




#### step 5: Determine proper parameters for ntree and mtry - Another step which can be refined later  - for now everything is commented out (6/29) ####

#### for loop for testing number of trees

#Determined that 1000 trees provided most stable error rate
