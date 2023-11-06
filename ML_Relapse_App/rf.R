# Random Forest Modeling
# Team Miami

# load required libraries
library(conflicted)
suppressMessages(conflict_prefer("filter", "dplyr"))
library(janitor) # for tabyl and adorn_* functions
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidymodels))
tidymodels_prefer()  # tell conflicted to prefer tidymodels packages
library(randomForest)
library(ranger)
library(usemodels)
library(gmodels)
library(vip)

# Make Cross Validation Object
cv_folds <-
  vfold_cv(analysis_train,
           v = 5,
           strata = relapse)
# Load data
source("load.R")
source("preprocess.R")

## Make a Recipe
relapse_rec_rf <- relapse_rec

relapse_rec_rf <-
  recipe(relapse ~ ., data = analysis_train)%>% 
  update_role(who, new_role = "ID")%>% 
  step_normalize(all_numeric_predictors())%>% 
  step_corr(all_numeric_predictors(), threshold = 0.7, method = "spearman") %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_factor_predictors())


#relapse_rec_rf

## Make the Random Forest Model
ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

ranger_workflow <-
  workflow() %>%
  add_recipe(relapse_rec_rf) %>%
  add_model(ranger_spec)

ranger_grid <-
  grid_regular(mtry(range = c(2, 6)), # number of predictors at each split
               min_n(range = c(10, 50)), # number of people need to keep splitting
               levels = 5
  )
set.seed(8701)
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow, resamples = cv_folds, grid = ranger_grid)
doParallel::stopImplicitCluster()

# AUC
rf_auc <- ranger_tune%>% 
  collect_metrics()%>%  
  filter(.metric == "roc_auc") %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry, values_to = "value", names_to = "parameter") %>%  
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") + 
  labs (x=NULL, y ="AUC") 

# Finalize Model
rf_best <- select_best(ranger_tune, "roc_auc")

rf_results <- ranger_workflow %>%
  finalize_workflow(rf_best) %>%
  last_fit(split = analysis_split,
           metrics = metric_set(recall, precision, f_meas, accuracy, kap, roc_auc, sens, yardstick::spec))

# Variable Importance Plot Training

rf_vip <- rf_results %>%
  extract_fit_parsnip() %>% 
  vip()


# The Prediction
the_prediction <- rf_results %>% 
  collect_predictions()
#head(the_prediction)

# Confusion Matrix Training

rf_CF_Matrix <- the_prediction %>% 
  conf_mat(relapse, .pred_class)

#print(rf_CF_Matrix)

# ROC Curve
rf_roc <- the_prediction %>%
  roc_curve(relapse, .pred_0) %>%
  autoplot()

#rf_roc

# Prediction Probabilities
rf_plot <- the_prediction %>%
  ggplot() +
  geom_density(
    aes(x = .pred_0,
        fill = relapse
    ),
    alpha = 0.5
  ) +
  labs(
    title = "True Relapse vs Prediction Probabilty",
    x = "Probabilty Of Relaspe",
    y = "Density"
  ) +
  ggthemes::theme_few() +
  theme(legend.title = element_blank())

#rf_plot

# Look at the Metrics
rf_mets <- rf_results %>%
  collect_metrics(summarize = TRUE)

#rf_mets

# The Prediction Training
train_rf <- ranger_workflow %>%
  finalize_workflow(rf_best)

train_rf %>%
  fit(analysis_train) %>%
  extract_fit_parsnip() %>%
  vip()

# *****Test Data*****
rf_test_pred <- 
  predict(train_rf %>%
            fit(analysis_train), analysis_test, type = "prob") %>% 
  bind_cols(analysis_test %>% select(relapse)) 

rf_test_curve <- rf_test_pred %>% 
  roc_curve(truth = relapse, .pred_0) %>% 
  autoplot()

rf_test_roc_auc <- rf_test_pred %>% 
  roc_auc(truth = relapse, .pred_0)

test_vip <-train_rf %>%
  fit(analysis_test) %>%
  extract_fit_parsnip() %>%
  vip()


rf_test_plot <- rf_test_pred %>%
  ggplot() +
  geom_density(
    aes(x = .pred_0,
        fill = relapse
    ),
    alpha = 0.5
  ) +
  labs(
    title = "True Relapse vs Prediction Probabilty",
    x = "Probabilty Of Relaspe",
    y = "Density"
  ) +
  ggthemes::theme_few() +
  theme(legend.title = element_blank())

# Save results
if (!dir.exists(paste0(here::here(),"/data"))) {
  dir.create(paste0(here::here(),"/data"))
}
saveRDS(rf_vip, file = "./data/rf_vip.rds")
saveRDS(rf_plot, file = "./data/rf_plot.rds")
saveRDS(rf_roc, file = "./data/rf_roc.rds")
saveRDS(rf_CF_Matrix, file = "./data/rf_cf.rds")
saveRDS(rf_mets, file = "./data/rf_mets.rds")
saveRDS(rf_results, file = "./data/rf_results.rds")
saveRDS(rf_auc, file = "./data/rf_auc.rds")
saveRDS(train_rf, file = "./data/rf_train_auc.rds")
# saveRDS(rf_auc, file = "./data/rf_auc.rds")



# Save Test Results

saveRDS(rf_test_curve, file = "./data/rf_test_curve.rds")
saveRDS(rf_test_roc_auc, file = "./data/rf_test_roc_auc.rds")
saveRDS(test_vip, file = "./data/test_vip.rds")
saveRDS(rf_test_pred, file = "./data/rf_test_pred.rds")
saveRDS(rf_test_plot, file = "./data/rf_test_plot.rds")


