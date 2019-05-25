# gganimate script from example data

# start with tifs
system("aws s3 sync s3://earthlab-amahood/data/summer19_model_results/May23_modelrun_w_precip/kings data/kings")

files <- list.files("data/kings", full.names = TRUE)


lst <- list() # we're gonna make a list of data frames then rbind them
for(i in 1:length(files)){
  r <- raster(files[i])
  # initially, the name of the column with the values is the filename.
  # here we need value column to be the same for each year so we can rbind at the end
  names(r) <- "value" 
  lst[[i]] <- r %>%
    as.data.frame(xy=TRUE) %>%
    mutate(year = i+1983) #hackjob method to get the year in there
}
df <- do.call("rbind",lst) # rbinding to one df

anim<-ggplot(df, aes(x=x,y=y,fill=value))+ 
  geom_raster() + # the business
  theme_void() + # this is a theme without any annoying lines  
  scale_fill_viridis_c(name = "class") + # setting the color scheme and naming the legend
  labs(title = 'Year: {frame_time}') + # this is setting the label on top
  transition_time(year) # using the year column as the time step

aa<-gganimate::animate(anim, fps=2, nframes = length(unique(df$year)))
anim_save(aa, filename="data/example.gif")

system(paste("aws s3 cp",
             "data/example.gif",
             "s3://earthlab-amahood/data/summer19_model_results/animations/kings_ex.gif"))
