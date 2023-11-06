--------------------------------------getting results 

########## Loading model files ###########

# Logistic model

# Lasso model 



# knn model




## load CART results ----

if (!file.exists("./data/cart_conf_mat.rds") |
    !file.exists("./data/cart_metrics.rds") |
    !file.exists("./data/cart_vip.rds") |
    !file.exists("./data/cart_tree_fit.rds")|
    !file.exists("./data/cart_roc_plot.rds")|
    !file.exists("./data/cart_prediction_plot.rds")
) {
  suppressMessages(library(rpart))
  suppressMessages(library(rpart.plot))
} else {
  load("./data/cart_conf_mat.rds")
  load("./data/cart_metrics.rds")
  load("./data/cart_vip.rds")
  load("./data/cart_tree_fit.rds")
  load("./data/cart_roc_plot.rds")
  load("./data/cart_prediction_plot.rds")
}




## load Random Forest model results -----

if (!file.exists("./data/rf_vip.rds") |
    !file.exists("./data/rf_plot.rds") |
    !file.exists("./data/rf_roc.rds") |
    !file.exists("./data/rf_cf.rds")|
    !file.exists("./data/rf_mets.rds")|
    !file.exists("./data/rf_results.rds")|
    !file.exists("./data/rf_auc.rds")
) {
  suppressMessages(library(rpart))
  suppressMessages(library(rpart.plot))
} else {
  load("./data/rf_vip.rds")
  load("./data/rf_plot.rds")
  load("./data/rf_roc.rds")
  load("./data/rf_cf.rds")
  load("./data/rf_mets.rds")
  load("./data/rf_results.rds")
}
