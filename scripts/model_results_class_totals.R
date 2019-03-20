# Step 1: Load packages ---------------------------
libs <- c("randomForest", "dplyr","sf", "caTools", "raster", "tidyverse", "ggplot2", "doParallel")
# lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
install.packages("randomForest")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#system("aws s3 sync s3://earthlab-amahood/data/mucc_model_results_allyears data/allyears_results")
#Step 2: load data ----
dir.create("data")
dir.create("data/allyears_results")
system ("aws s3 sync s3://earthlab-amahood/data/mucc_ensemble_results_done data/allyears_results")
all_years_files <- list.files("data/allyears_results", full = T)

system("aws s3 sync s3://earthlab-amahood/data/annual_esp_masks_mucc/ data/esp_masks/")
system("aws s3 sync s3://earthlab-amahood/data/annual_urb_masks_mucc/ data/urb_masks/")
system("aws s3 sync s3://earthlab-amahood/data/fire_perimeters/ data/fire_perims/")
system("aws s3 sync s3://earthlab-amahood/data/ls5_mucc_extent_matched/ data/extent_matched/")
#step 3: extract class totals ----
results_list <- list()
total_na <- raster("data/urb_masks/inclusivebinary_allyears_total_nacount.tif")

for(i in 1:length(all_years_files)) {
  results  <- raster(all_years_files[i])
  results <- raster::extend(results, total_na)
  results <- crop(results, total_na)
  results_list[i] <- results
}

# 
# df <- bind_rows( 
#     r1984 = as_data_frame(table(values(results_list[[1]])))
#   , r1985 = as_data_frame(table(values(results_list[[2]])))
#   , r1986 = as_data_frame(table(values(results_list[[3]])))
#   , r1987 = as_data_frame(table(values(results_list[[4]])))
#   , r1988 = as_data_frame(table(values(results_list[[5]])))
#   , r1989 = as_data_frame(table(values(results_list[[6]])))
#   , r1990 = as_data_frame(table(values(results_list[[7]])))
#   , r1991 = as_data_frame(table(values(results_list[[8]])))
#   , r1992 = as_data_frame(table(values(results_list[[9]])))
#   , r1993 = as_data_frame(table(values(results_list[[10]])))
#   , r1994 = as_data_frame(table(values(results_list[[11]])))
#   , r1995 = as_data_frame(table(values(results_list[[12]])))
#   , r1996 = as_data_frame(table(values(results_list[[13]])))
#   , r1997 = as_data_frame(table(values(results_list[[14]])))
#   , r1998 = as_data_frame(table(values(results_list[[15]])))
#   , r1999 = as_data_frame(table(values(results_list[[16]])))
#   , r2000 = as_data_frame(table(values(results_list[[17]])))
#   , r2001 = as_data_frame(table(values(results_list[[18]])))
#   , r2002 = as_data_frame(table(values(results_list[[19]])))
#   , r2003 = as_data_frame(table(values(results_list[[20]])))
#   , r2004 = as_data_frame(table(values(results_list[[21]])))
#   , r2005 = as_data_frame(table(values(results_list[[22]])))
#   , r2006 = as_data_frame(table(values(results_list[[23]])))
#   , r2007 = as_data_frame(table(values(results_list[[24]])))
#   , r2008 = as_data_frame(table(values(results_list[[25]])))
#   , r2009 = as_data_frame(table(values(results_list[[26]])))
#   , r2010 = as_data_frame(table(values(results_list[[27]])))
#   , r2011 = as_data_frame(table(values(results_list[[28]])))
#   , .id = "raster"
# )
# 
# 
# 
# df <- df %>% dplyr::select(raster, class = Var1,  pixel_count = n)


#attempting to take masked pixels out of the total to get accurate percentages for classes ----
# esp_na <- raster("data/esp_masks/binary_allyears_esp_nacount.tif")
# urb_na <- raster("data/urb_masks/binary_allyears_urb_nacount.tif")
# scene_na <- raster("data/urb_masks/binary_allyears_scene_nacount.tif")

total_na <- raster("data/urb_masks/inclusivebinary_allyears_total_nacount.tif")
matched_extent <- ("data/extent_matched/trimmed_extent_p42r31.gri")

cores <- 6
registerDoParallel(cores)

masked_results_list <- list()
for(i in 1:length(results_list)) {
  # esp_mask <- projectRaster(esp_mask, i, res = 30)
  # masked <- raster::extend(results_list[[i]], total_na)
  # masked <- raster::crop(masked, total_na)
  masked <- results_list[[i]]
  masked_results_list[[i]] <- masked
}

for(i in 1:length(masked_results_list)) {
  masked <- mask(masked_results_list[[i]], total_na, maskvalue = 0)
  masked_results_list[[i]] <- masked
}

# foreach(i = masked_results_list) %dopar% {
#   # esp_mask <- projectRaster(esp_mask, i, res = 30)
#   masked <- mask(i, esp_na, maskvalue = 0)
#   masked_results_list[i] <- masked
# }
#trying to make a better organized pixel counts table ----
results_list <- masked_results_list

df2 <- rbind( 
  r1984 = as.vector(table(values(results_list[[1]])))
  , r1985 = as.vector(table(values(results_list[[2]])))
  , r1986 = as.vector(table(values(results_list[[3]])))
  , r1987 = as.vector(table(values(results_list[[4]])))
  , r1988 = as.vector(table(values(results_list[[5]])))
  , r1989 = as.vector(table(values(results_list[[6]])))
  , r1990 = as.vector(table(values(results_list[[7]])))
  , r1991 = as.vector(table(values(results_list[[8]])))
  , r1992 = as.vector(table(values(results_list[[9]])))
  , r1993 = as.vector(table(values(results_list[[10]])))
  , r1994 = as.vector(table(values(results_list[[11]])))
  , r1995 = as.vector(table(values(results_list[[12]])))
  , r1996 = as.vector(table(values(results_list[[13]])))
  , r1997 = as.vector(table(values(results_list[[14]])))
  , r1998 = as.vector(table(values(results_list[[15]])))
  , r1999 = as.vector(table(values(results_list[[16]])))
  , r2000 = as.vector(table(values(results_list[[17]])))
  , r2001 = as.vector(table(values(results_list[[18]])))
  , r2002 = as.vector(table(values(results_list[[19]])))
  , r2003 = as.vector(table(values(results_list[[20]])))
  , r2004 = as.vector(table(values(results_list[[21]])))
  , r2005 = as.vector(table(values(results_list[[22]])))
  , r2006 = as.vector(table(values(results_list[[23]])))
  , r2007 = as.vector(table(values(results_list[[24]])))
  , r2008 = as.vector(table(values(results_list[[25]])))
  , r2009 = as.vector(table(values(results_list[[26]])))
  , r2010 = as.vector(table(values(results_list[[27]])))
  , r2011 = as.vector(table(values(results_list[[28]])))
)

df2 <- as_data_frame(df2)
names(df2) <- c("grass1", "grass2", "shrub2", "shrub1")

na_value_list <- list()
for(i in 1:length(masked_results_list)) {
  na_value_list[i] <- as.vector(sum(values(is.na(masked_results_list[[i]]))))
}

df2 <- mutate(df2, 
              na_pixel_count = as.numeric(na_value_list))



df2 <- df2 %>% mutate(year = c(1984:2011),
                      total_pixels = as.numeric(grass1 + grass2 + shrub1 + shrub2 + na_pixel_count),
                      total_nonna = as.numeric(grass1 + grass2 + shrub1 + shrub2),
                      total_study_area = as.numeric(grass1 + grass2 + shrub1 + shrub2),
                      percent_grass_certain = as.numeric((grass1 / total_study_area) * 100),
                      percent_grass_likely = as.numeric((grass2 / total_study_area) * 100),
                      percent_shrub_certain = as.numeric((shrub1 / total_study_area) * 100),
                      percent_shrub_likely = as.numeric((shrub2 / total_study_area) * 100),
                      percent_shrub_total = as.numeric(percent_shrub_certain + percent_shrub_likely),
                      percent_grass_total = as.numeric(percent_grass_certain + percent_grass_likely),
                      percent_na = as.numeric((na_pixel_count / total_pixels) * 100),
                      percent_check = as.numeric(percent_grass_total + percent_shrub_total + percent_na),
                      shrubvgrass = as.numeric(grass1  / shrub1),
                      shrubgrassdiff = as.numeric(percent_shrub_total - percent_grass_total)
)

write.csv(df2, file = "data/results_inclusive75_3_20_2019.csv")
system("aws s3 cp data/results_inclusive75_3_20_2019.csv s3://earthlab-amahood/data/")

#pulling in already completed results csv

system("aws s3 cp s3://earthlab-amahood/data/results_3_12_2019.csv data/results_3_12_2019.csv")
df2 <- read.csv("data/results_3_12_2019.csv/results_3_12_2019.csv")


fit <- lm(df2$percent_shrub_total ~ df2$year)
coef  <- coefficients(fit)       # coefficients
resid <- residuals(fit)          # residuals
pred  <- predict(fit)            # fitted values
rsq   <- summary(fit)$r.squared  # R-sq for the fit
se    <- summary(fit)$sigma  

ggplot(data=df2) + 
aes(x = df2$year) + 
geom_point(aes(y=df2$percent_shrub_total), color = 'darkgreen') + 
geom_smooth(aes(y=df2$percent_shrub_total), color = 'darkgreen', method = "lm") +
xlab("year") +
ylab("Sagebrush %") +
ylim(0, 50)


write.csv(df2)

#plotting shrub and grass totals together
ggplot(data=df2) + geom_point(aes(y=df2$percent_grass_total, x = df2$year), color = 'yellow') + geom_smooth(aes(y=df2$percent_grass_total, x = df2$year), method = "lm", color = 'yellow') +
geom_point(aes(y=df2$percent_shrub_total, x = df2$year), color = 'green') + geom_smooth(aes(y=df2$percent_shrub_total, x = df2$year), method = "lm", color = 'green') +
ylim(0, 50)

#filtering out years with >2 sd variability in either class
df3 <- filter(df2, (percent_shrub_total - mean(percent_shrub_total)) < 1.5 * sd(percent_shrub_total)) %>%
  filter((percent_grass_total - mean(percent_grass_total)) < 1.5 * sd(percent_grass_total))

#scatter plot of cheat and sage totals with linear trend line ----
ggplot(data=df, aes(x=raster, y=n, group = Var1, colour = as.factor(Var1))) + geom_point() 
+ geom_smooth(method = "lm")
+ geom_line(x)

#begin work here (1/23) for getting table of combined counts for high and low certainty ensemble results ----
r2011b <- bind_rows(r2011[1, 2] + r2011[2,2], r2011[3,2] + r2011[4,2])





####working with fire perimeter data (2/21) ####
fire_perims <- st_read("data/fire_perims/dissolve_mtbs_perims_1984-2015_DD_20170501.shp")
fire_perims <- st_transform(fire_perims, as.character(crs(results_list[[1]])))
scene_poly <- st_make_grid(results_list[[1]])
fire_perims <- st_crop(fire_perims, scene_poly)

years <- c(1984:2011)
class_totals_twoyr <- list()
class_totals_threeyr <- list()
class_totals_fouryr <- list()

#cores <- detectCores()
#registerDoParallel(cores)
iterator <- c(1:28)

annual_oneyrpostburn <- list()

for(i in 1:length(iterator)) {
  fire_perims2 <- fire_perims %>% filter(Year == years[i])
  k = i + 1 #counter
  result_year <- names(results_list[[k]])
  fire_names <- seq(length((fire_perims2$Fire_Name)))
   
  class_totals_oneyr <- raster::extract(results_list[[k]], y = fire_perims2, df = T)
  oneyr_df <- list()
  for(j in 1:length(fire_names)) {
    fire <- class_totals_oneyr[class_totals_oneyr$ID == j,] %>% dplyr::select(-ID)
    class0 <- sum(fire[,1] == 0, na.rm = T)
    class1 <- sum(fire[,1] == 1, na.rm = T)
    class2 <- sum(fire[,1] == 2, na.rm = T)
    class3 <- sum(fire[,1] == 3, na.rm = T)
    grass <- class0 + class1
    sage <- class2 + class3
    fire_stats <- c(grass,sage) 
    oneyr_df[[j]] <- fire_stats
  }
  annual_oneyrpostburn[[i]] <- oneyr_df
  names(annual_oneyrpostburn[[i]]) <- years[i]
}
  




# class_totals_twoyr[i] <- raster::extract(results_list[[(i + 2)]], y = fire_perims2)
# class_totals_threeyr[i] <- raster::extract(results_list[[(i + 3)]], y = fire_perims2)
# class_totals_fouryr[i] <- raster::extract(results_list[[(i + 4)]], y = fire_perims2)


fire_perims2 <- fire_perims %>% filter(Year == 2007) 
fire_perims2 <- st_transform(fire_perims2, as.character(crs(results_list[[1]])))
st_make_grid(results_list[[1]])
fire_perims2 <- st_crop(fire_perims2, scene_poly)

class_totals_burned_2007 <- raster::extract(results_list[[26]], y = fire_perims2)
names(class_totals_burned_2007) <- fire_perims2$Fire_Name
ggplot(df2, aes(year)) + geom_smooth(aes(y = percent_shrub_total, color = "green"), method = "lm", se = F) + geom_smooth(aes(y = percent_grass_total, color = "yellow"), method = "lm", se = F) + geom_point(aes(y=df2$percent_shrub_total, color = "green")) + geom_point(aes(y=df2$percent_grass_total, color = "yellow")) 
