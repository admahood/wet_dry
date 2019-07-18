system("aws s3 cp s3://earthlab-amahood/data/vegindices_table.csv data/table_data")

veg_indices_data <- read.csv("data/table_data/vegindices_table.csv", stringsAsFactors = F, header = T)
veg_indices_data <- veg_indices_data %>% dplyr::select(-X)


system("aws s3 cp s3://earthlab-amahood/data/tasseledcap_table.csv data/table_data")

tasseledcap_data <- read.csv("data/table_data/tasseledcap_table.csv", stringsAsFactors = F, header = T)
tasseledcap_data <- tasseledcap_data %>% dplyr::select(-X)

system("aws s3 cp s3://earthlab-amahood/data/landsatbands_table.csv data/table_data")

landsatbands_data <- read.csv("data/table_data/landsatbands_table.csv", stringsAsFactors = F, header = T)



