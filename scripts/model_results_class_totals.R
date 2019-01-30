# Step 1: Load packages ---------------------------
libs <- c("randomForest", "dplyr","sf", "caTools", "raster", "tidyverse", "ggplot2")
# lapply(libs, install.packages, character.only = TRUE, verbose = FALSE)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

#system("aws s3 sync s3://earthlab-amahood/data/mucc_model_results_allyears data/allyears_results")
#Step 2: load data ----
dir.create("data/allyears_results")
system ("aws s3 sync s3://earthlab-amahood/data/mucc_ensemble_results_done data/allyears_results")
all_years_files <- list.files("data/allyears_results", full = T)


#step 3: extract class totals ----
results_list <- list()

for(i in 1:length(all_years_files)) {
 results_list[i]  <- raster(all_years_files[i])
}

df <- bind_rows( 
    r1984 = as_data_frame(table(values(results_list[[1]])))
  , r1985 = as_data_frame(table(values(results_list[[2]])))
  , r1986 = as_data_frame(table(values(results_list[[3]])))
  , r1987 = as_data_frame(table(values(results_list[[4]])))
  , r1988 = as_data_frame(table(values(results_list[[5]])))
  , r1989 = as_data_frame(table(values(results_list[[6]])))
  , r1990 = as_data_frame(table(values(results_list[[7]])))
  , r1991 = as_data_frame(table(values(results_list[[8]])))
  , r1992 = as_data_frame(table(values(results_list[[9]])))
  , r1993 = as_data_frame(table(values(results_list[[10]])))
  , r1994 = as_data_frame(table(values(results_list[[11]])))
  , r1995 = as_data_frame(table(values(results_list[[12]])))
  , r1996 = as_data_frame(table(values(results_list[[13]])))
  , r1997 = as_data_frame(table(values(results_list[[14]])))
  , r1998 = as_data_frame(table(values(results_list[[15]])))
  , r1999 = as_data_frame(table(values(results_list[[16]])))
  , r2000 = as_data_frame(table(values(results_list[[17]])))
  , r2001 = as_data_frame(table(values(results_list[[18]])))
  , r2002 = as_data_frame(table(values(results_list[[19]])))
  , r2003 = as_data_frame(table(values(results_list[[20]])))
  , r2004 = as_data_frame(table(values(results_list[[21]])))
  , r2005 = as_data_frame(table(values(results_list[[22]])))
  , r2006 = as_data_frame(table(values(results_list[[23]])))
  , r2007 = as_data_frame(table(values(results_list[[24]])))
  , r2008 = as_data_frame(table(values(results_list[[25]])))
  , r2009 = as_data_frame(table(values(results_list[[26]])))
  , r2010 = as_data_frame(table(values(results_list[[27]])))
  , r2011 = as_data_frame(table(values(results_list[[28]])))
  , .id = "raster"
)

df <- df %>% dplyr::select(raster, class = Var1,  pixel_count = n)

#trying to make a better organized pixel counts table
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

df2 <- df2 %>% mutate(year = c(1984:2011),
                      percent_grass_certain = as.numeric(grass1 / (grass1 + grass2 +shrub1 + shrub2) * 100),
                      percent_grass_likely = as.numeric(grass2 / (grass2 + grass1 +shrub1 + shrub2) * 100),
                      percent_shrub_certain = as.numeric(shrub1 / (shrub1 + grass2 +grass1 + shrub2) * 100),
                      percent_shrub_likely = as.numeric(shrub2 / (shrub2 + grass2 +shrub1 + grass1) * 100),
                      percent_shrub_total = as.numeric(percent_shrub_certain + percent_shrub_likely),
                      percent_grass_total = as.numeric(percent_shrub_certain + percent_shrub_likely))

ggplot(data=df2, aes(y= df2$percent_shrub_total, x = df2$year)) + geom_point() + geom_smooth(method = "lm")

#scatter plot of cheat and sage totals with linear trend line ----
ggplot(data=df, aes(x=raster, y=n, group = Var1, colour = as.factor(Var1))) + geom_point() + geom_smooth(method = "lm")

#begin work here (1/23) for getting table of combined counts for high and low certainty ensemble results ----
r2011b <- bind_rows(r2011[1, 2] + r2011[2,2], r2011[3,2] + r2011[4,2])