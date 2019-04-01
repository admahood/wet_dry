library(dplyr, knitr, kableExtra)

mtry <- seq(1,8,1) # 22 = # cols in the yet to be created training set
sc <- seq(4,25,1)
nodesize <- seq(1,4,1)
elevation <- c("yes", "no")

hyper_grid <- expand.grid(mtry = mtry, 
                          sc=sc, 
                          nodesize = nodesize,
                          elevation = elevation) ; nrow(hyper_grid)

hypergrid_ex <- head(hyper_grid, n = 16)

system("aws s3 sync s3://earthlab-amahood/data/hypergrids_vb data/hypergrids")

hypergrid_results <- read.csv("data/hypergrids/hgOct_16_2018.csv")

hypergrid_ex_results <- head(hypergrid_results, n = 10) %>% dplyr::select(-X, -accuracy_p, -mcnemar_p, -ac_lower, -ac_upper, -ac_null, -prevalence, -balanced_accuracy, -pos_pred_value, -neg_pred_value)
