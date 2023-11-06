library(tidyverse)

# Cart output

cart_conf_mat_rds <-  read_rds(file ="./data/cart_conf_mat.rds") %>% 
  autoplot(type = "heatmap")
cart_roc_plot_rds <- read_rds( file = "./data/cart_roc_plot.rds")
cart_metrics_rds<- read_rds(file = "./data/cart_matrics.rds")


cart_vip_rds <- read_rds(file = "./data/cart_vip.rds")
cart_prediction_plot_rds <- read_rds(file = "./data/cart_prediction_plot.rds")

cart_tree_fit_rds <- read_rds( file = "./data/cart_tree_fit.rds")


# knn output

knn_matrices_rds <- read_rds(file = "./data/knn_matrices.rds")
knn_conf_mat_rds <- read_rds(file = "./data/knn_conf_mat.rds") %>% 
  autoplot(type = "heatmap")
knn_pred_plot_rds <- read_rds(file = "./data/knn_pred_plot.rds")
replace_roc_plot_rds <- read_rds(file = "./data/knn_roc_plot.rds")


# Random Forest 

rf_vip_rds <- read_rds(file = "./data/rf_vip.rds")
rf_plot_rds <- read_rds(file = "./data/rf_plot.rds")
rf_roc_rds <-  read_rds(file = "./data/rf_roc.rds")
rf_CF_Matrix_rds <- read_rds(file = "./data/rf_cf.rds") %>% 
  autoplot(type = "heatmap")
rf_mets_rds <- read_rds(file = "./data/rf_mets.rds")
#rf_results_rds <- read_rds(file = "./data/rf_results.rds")
rf_auc_rds <- read_rds(file = "./data/rf_auc.rds")
#train_rf_auc_rds <- read_rds(file = "./data/rf_train_auc.rds")


# Lasso 

vip_lasso_overall_rds <- read_rds(file = "./data/vip_overall.rds")
Pred_plot_lasso_rds <- read_rds(file = "./data/Pred_plot_lasso.rds")
roc_lasso_plot_rds <- read_rds(file = "./data/roc_lasso_plot.rds")
Lasso_metrics_rds <- read_rds(file = "./data/lasso_matrics.rds")
lasso_conf_mat_rds <- read_rds(file = "./data/lasso_conf_mat.rds") %>% 
  autoplot(type = "heatmap")


# Logistic

logistic_betas_rds <- read_rds(file = "./data/logistic_betas.rds")
logistic_pred_plot_rds <- read_rds(file = "./data/logistic_pred_plot.rds")
logistic_ROC_rds <- read_rds(file = "./data/logistic_ROC.rds") 
logistic_matrices_rds <- read_rds(file = "./data/logistic_matrices.rds")
logistic_conf_mat_rds <- read_rds(file = "./data/logistic_conf_mat.rds") %>% 
  autoplot(type = "heatmap")


# Comparison Table 

com_table <-  
  left_join(Lasso_metrics_rds,
            logistic_matrices_rds, 
            by =".metric") %>% 
  left_join(.,knn_matrices_rds, by =".metric") %>% 
  left_join(.,cart_metrics_rds, by =".metric") %>% 
  left_join(.,rf_mets_rds, by =".metric") %>% 
  select(".metric", ".estimate.x", ".estimate.y", ".estimate.x.x", ".estimate.y.y", ".estimate") %>% 
  rename(Metric =".metric", Lasso = ".estimate.x", Logistic = ".estimate.y", KNN =".estimate.x.x", CART = ".estimate.y.y", "Random Forest" =".estimate")


# Testing Results

rf_test_curve_rds <-  read_rds(file = "./data/rf_test_curve.rds")
rf_test_roc_auc_rds <- read_rds(file = "./data/rf_test_roc_auc.rds")
test_vip_rds <- read_rds(file = "./data/test_vip.rds")
#rf_test_pred_rds <- read_rds (file = "./data/rf_test_pred.rds")
rf_test_plot_rds <- read_rds(file = "./data/rf_test_plot.rds")