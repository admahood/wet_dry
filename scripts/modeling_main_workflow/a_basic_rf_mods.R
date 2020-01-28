# random forest modelling code =================================================
# jan2020
# d murphy & a mahood

# setup ========================================================================

libs <- c("randomForest", "dplyr","sf", "caTools", "dplyr", "caret", "VSURF")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
#lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)         # - optional line to install packages
source("scripts/functions.R")
set.seed(11)

training_s3_path <- "s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_ndvi_informed"
system(paste0("aws s3 sync ", training_s3_path, " data/training_timeseries")) # Use May 21 splits (most recent)

system(paste("aws s3 cp",
             "s3://earthlab-amahood/wet_dry/derived_vector_data/training_time_series_ndvi_informed",
             "data/training_points/ndvi_informed_06-10.gpkg"))

# data import and wrangling ====================================================
gtrain <- st_read("data/training_points/ndvi_informed_06-10.gpkg") %>%
  dplyr::mutate(spring_ndsvi = get_ndsvi(band3 = spring_sr_band3, 
                                         band5 = spring_sr_band5),
                summer_ndsvi = get_ndsvi(band3 = summer_sr_band3, 
                                         band5 = summer_sr_band5),
                spring_satvi = get_satvi(band3 = spring_sr_band3, 
                                         band5 = spring_sr_band5,
                                         band7 = spring_sr_band7, L = 0.5),
                summer_satvi = get_satvi(band3 = summer_sr_band3, 
                                         band5 = summer_sr_band5, 
                                         band7 = summer_sr_band7, L = 0.5),
                folded_aspect = get_folded_aspect(aspect = aspect),
                Year = as.integer(as.character(Year))) %>%
  dplyr::select(-aspect, -def_z) %>%
  st_set_geometry(NULL)

# basic modeling ===============================================================
model2 <- randomForest(Label ~ . ,
                       data = gtrain %>% dplyr::select(-Year), 
                       ntree = 1000, 
                       mtry = 10, 
                       nodesize = 4)

varImpPlot(model2)

wet_mod <- randomForest(Label ~ . ,
                        data = gtrain %>% filter(Year > 2009 | Year < 2007), 
                        ntree = 1000, 
                        mtry = 10, 
                        nodesize = 4)
varImpPlot(wet_mod)  

# model trimming ===============================================================
# ddd is a list of data frames
# mods is a list of models
# each iteration drops the least important variable from the data frame and
# trains a new model, continuing until nothing is left

ddd <- list()
mods <-list()

ddd[[1]] <- gtrain%>% 
  filter(Year > 2009 ) %>%
  dplyr::select(-Year)
rvars <- ncol(ddd[[1]])-2

control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3,
                        search="grid")
var_results <- data.frame(accuracy=NA, 
                          nvars = NA, 
                          # mean_acc=NA,
                          # sd_acc=NA, 
                          dropped = NA)

for (i in 1:rvars){ # this takes 10-30 minutes
  t0<-Sys.time()
  # tgrid <- expand.grid(
  #   .mtry = 1:round(sqrt(ncol(ddd[[i]])-2)), #cluster and geom don't count
  #   .splitrule = "gini",
  #   .min.node.size = c(10, 20)
  # )
  # next time this is ran, do rf[[i]] to be able to look at the models later
  # mods[[i]] <- train(binary~., 
  #                    data=ddd[[i]],#st_set_geometry(ddd[[i]], NULL), 
  #                    method='ranger',
  #                    metric=c('Accuracy'), # or RMSE?
  #                    tuneGrid=tgrid, 
  #                    trControl=control,
  #                    #case.weights = www, # not sure why this doesn't work
  #                    importance = "permutation")
  
  mods[[i]] <- randomForest(Label ~ .,
               data = ddd[[i]] , 
               ntree = 100, 
               mtry = 10, 
               nodesize = 4)
  
  var_results[i, 1] <- mods[[i]]$err.rate[100,1]
  var_results[i, 2] <- i

  
  # var_results[i, 1] <- max(mods[[i]]$results$Accuracy)
  # var_results[i, 2] <- i
  # var_results[i, 3] <- mean(mods[[i]]$results$Accuracy)
  # var_results[i, 4] <- sd(mods[[i]]$results$Accuracy)
  
  
  # least_important <- caret::varImp(mods[[i]])$importance %>%
  #   rownames_to_column("var") %>%
  #   arrange(Overall)
  least_important <- mods[[i]]$importance %>%
    as_tibble(rownames = "var") %>%
    filter(MeanDecreaseGini == min(.$MeanDecreaseGini))%>%
    dplyr::select(var) %>%
    pull()
  
  vvv <- least_important
  ddd[[i+1]] <- dplyr::select(ddd[[i]],-vvv)
  
  var_results[i, 3] <- vvv
  # var_results[i, 5] <- vvv
  print(paste("Progress:", round(i/rvars*100), "% |",
              "Accuracy:",  round(max(mods[[i]]$results$Accuracy)*100),
              "% | Dropped", vvv, Sys.time()-t0))
}

var_results

ggplot(var_results, aes(x=nvars-1, y=accuracy)) + 
  geom_line()

best <- 12

# then make a nice plot
names(ddd[[best]]) -> nnn
nnn <- nnn[-1]
imp <- varImp(mods[[best]])
ggplot(var_results, aes(x=nvars-1, y=accuracy)) + 
  geom_line() +
  geom_line(aes(y=accuracy), col = "red", lwd=1) +
  # geom_annotate("text", label = dropped) + #something like this
  xlab("# Variables dropped")# +
  # geom_vline(xintercept = best-1, lty=3) +
  # annotate("text",x=0, y=var_results$accuracy[best]-0.02, hjust=0, vjust=1,
  #          label = paste("Remaining Variables: \n", 
  #                        paste(nnn, collapse = "\n")
  #                        ," \nmtry",mods[[best]]$bestTune[[1]],
  #                        " \nmin.node.size", mods[[best]]$bestTune[[3]]
  #          )) +
  # ggsave("var_dropping_w_vegbank.pdf")
