# gganimate script from example data

#start with tifs
system("aws s3 sync s3://earthlab-amahood/data/summer19_model_results/May23_modelrun_w_precip/kings data/kings")

# read in and convert to data frame
files <- list.files("data/kings", full.names = TRUE)

lst <- list()
for(i in 1:length(files)){
  r <- raster(files[i])
  names(r) <- "value"
  lst[[i]] <- r %>%
    as.data.frame(xy=TRUE) %>%
    mutate(year = i+1983) #hackjob, obviously
}
df <- do.call("rbind",lst)

anim<-ggplot(df, aes(x=x,y=y,fill=value))+
  geom_raster() +
  theme_void() +    
  scale_fill_viridis_c(name = "class") +
  labs(title = 'Year: {frame_time}') +
  transition_time(year)

aa<-gganimate::animate(anim, fps=2, nframes = length(unique(df$year)))
anim_save(aa, filename="data/example.gif")
