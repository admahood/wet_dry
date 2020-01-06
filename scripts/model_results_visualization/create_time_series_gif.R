# Title: Create .gifs from time series results for quick viewing
# Authors: Dylan Murphy, Adam Mahood
# Date Created:  1/5/20
# Date Modified: ""

#Load Packages
anim_libs <- c("gganimate","gifski")
lapply(anim_libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(anim_libs, library, character.only = TRUE, verbose = FALSE)

#list land cover classifications present in results folder (after applying the model)
lcc_rasters <- list.files("data/results", pattern = "\\.tif$", full.names = T)

#change years to match years classified
years=2006:2010
ts_df=list()

#create data frame of raster predictions to feed into gganimate
for (i in 1:length(years)) {
  rrr <- raster(lcc_rasters[i])
  rr <- as.data.frame(rrr, xy = TRUE)
  names(rr) <- c("x","y", "Prediction")
  nn <- lcc_rasters[i]
  rr$year=as.numeric(years[i])
  ts_df[[i]]<-rr
}

ts_df<-do.call("rbind",ts_df)

#create series of ggplots showing model result rasters
anim<-ggplot(ts_df, aes(x=x,y=y,fill=as.numeric(Prediction)))+
  geom_raster() +
  theme_void() +
  scale_fill_viridis_c() +
  coord_fixed()+
  labs(title = 'Year: {frame_time}') +
  transition_time(year)

#combine individual ggplots into one gif animation
aa<-gganimate::animate(anim, fps=1, nframes = length(years))

#save locally
anim_save(aa, filename = "data/ard_climate_zscores_2class_wmuc_Jan2.gif")

#upload to s3
system("aws s3 cp data/ard_climate_zscores_2class_wmuc_Jan2.gif s3://earthlab-amahood/wet_dry/model_results/summer19_model_results/gifs/ard_climate_zscores_2class_wmuc_Jan2.gif")
