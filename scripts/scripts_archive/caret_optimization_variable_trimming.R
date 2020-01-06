# start with gbd_new as your initial data frame


ddd<- list()
mods<-list()
#ddd[[1]] <- cbind(clm,dplyr::select(resp,cluster)) 
ddd[[1]] <- gbd_new
rvars <- ncol(ddd[[1]])-2

# also need to try this with vbd only

control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3,
                        search="grid")
var_results <- data.frame(accuracy=NA, nvars = NA, mean_acc=NA,sd_acc=NA, dropped = NA)
for (i in 1:rvars){ # this takes 10-30 minutes
  t0<-Sys.time()
  tgrid <- expand.grid(
    .mtry = 1:round(sqrt(ncol(ddd[[i]])-2)), #cluster and geom don't count
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  # next time this is ran, do rf[[i]] to be able to look at the models later
  mods[[i]] <- train(cluster~., 
                     data=ddd[[i]],#st_set_geometry(ddd[[i]], NULL), 
                     method='ranger',
                     metric=c('Accuracy'), # or RMSE?
                     tuneGrid=tgrid, 
                     trControl=control,
                     #case.weights = www, # not sure why this doesn't work
                     importance = "permutation")
  
  var_results[i, 1] <- max(mods[[i]]$results$Accuracy)
  var_results[i, 2] <- i
  var_results[i, 3] <- mean(mods[[i]]$results$Accuracy)
  var_results[i, 4] <- sd(mods[[i]]$results$Accuracy)
  
  
  least_important <- caret::varImp(mods[[i]])$importance %>%
    rownames_to_column("var") %>%
    arrange(Overall)
  vvv <- least_important[1,1]
  ddd[[i+1]] <- dplyr::select(ddd[[i]],-vvv)
  
  var_results[i, 5] <- vvv
  print(paste("Progress:", round(i/rvars*100), "% |",
              "Accuracy:",  round(max(mods[[i]]$results$Accuracy)*100),
              "% | Dropped", vvv, Sys.time()-t0))
}

# first visually inspect and find the sweet spot
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
  geom_line(aes(y=mean_acc), col = "red", lwd=1) +
  # geom_annotate("text", label = dropped) + #something like this
  xlab("# Variables dropped") +
  geom_vline(xintercept = best-1, lty=3) +
  annotate("text",x=0, y=var_results$accuracy[best]-0.02, hjust=0, vjust=1,
           label = paste("Remaining Variables: \n", 
                         paste(nnn, collapse = "\n")
                         ," \nmtry",mods[[best]]$bestTune[[1]],
                         " \nmin.node.size", mods[[best]]$bestTune[[3]]
           )) +
  ggsave("var_dropping_w_vegbank.pdf")
