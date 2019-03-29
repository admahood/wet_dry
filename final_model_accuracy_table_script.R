install.packages("dplyr")
library(knitr, dplyr)

dir.create(table_data)
system("aws s3 cp s3://earthlab-amahood/data/thesis_final_models_accuracy_table.csv data/table_data")

data <- read.csv("data/table_data/thesis_final_models_accuracy_table.csv", stringsAsFactors = F, header = T)
data <- as.tibble(data) %>% dplyr::select(-X, -elevation, - oob_balanced, - ac_null, -ac_upper, -ac_lower, -mcnemar_p, -pos_pred_value, -neg_pred_value, -F1, -prevalence, -balanced_accuracy) 
model_names <- c("Shrub Over-predictor", "Grass Over-predictor", "Balanced")

hypergrid_metric_names <- c("mtry", "nodesize", "sc", "OOB error", "OOB error: grass", "OOB error: shrub", "Accuracy (validation)", "Kappa", "Accuracy p-value", "Sensitivity", "Specificity", "Precision", "Recall", "Detection Rate", "Detection Prevalence")
digits <- c(1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
 for(i in 1:length(digits)) { 
     data[,i] <- round(data[, i], digits[i])
     }
 data <- as.tibble(base::t(data))
 rownames(data) <- hypergrid_metric_names
 