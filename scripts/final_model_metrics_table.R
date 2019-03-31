library(dplyr)
library(knitr)
install.packages("kableExtra")
library(kableExtra)

system("aws s3 cp s3://earthlab-amahood/data/thesis_final_models_accuracy_table.csv data/table_data")
system("aws s3 cp s3://earthlab-amahood/data/results_inclusive75_3_20_2019.csv data/table_data")

data <- read.csv("data/table_data/thesis_final_models_accuracy_table.csv", stringsAsFactors = F, header = T)
data <- as.tbl(data) %>% dplyr::select(-X, -elevation, - oob_balanced, - ac_null, -ac_upper, -ac_lower, -mcnemar_p, -pos_pred_value, -neg_pred_value, -F1, -prevalence, -balanced_accuracy, -accuracy_p) 
model_names <- c("RF Model 1: Shrub Over-predictor", "RF Model 2: Grass Over-predictor", "RF Model 3: Balanced")

hypergrid_metric_names <- c("mtry", "nodesize", "sc", "OOB error", "OOB error: grass", "OOB error: shrub", "Accuracy (validation)", "Kappa", "Sensitivity", "Specificity", "Precision", "Recall", "Detection Rate", "Detection Prevalence")
digits <- c(1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
for(i in 1:length(digits)) { 
  data[,i] <- round(data[, i], digits[i])
}

data <- as.data.frame(base::t(data))
rownames(data) <- hypergrid_metric_names
kable(data, col.names = model_names, row.names = T) %>%
kable_styling(bootstrap_options = "striped", full_width = F)
