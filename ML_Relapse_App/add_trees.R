# Decision trees

# load source file
source("load.R")
source("preprocess.R")

# Fitting Model
cart_model <-
  decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")
cart_workflow <-
  workflow() %>%
  add_recipe(relapse_rec) %>%
  add_model(cart_model)

# Find best CC
doParallel::registerDoParallel() # use multiple CPU cores for faster processing
cart_tune <-
  cart_workflow %>%
  tune_grid(resamples = cv_folds,
            grid = 10,
            metrics = metric_set(accuracy, roc_auc, kap, sensitivity, specificity),
            control = control_grid(save_pred = TRUE)
  )
doParallel::stopImplicitCluster()

grid <- grid_regular(cost_complexity(), levels = 10)
#grid

# Search for the best Cp
show_best(cart_tune, metric = "roc_auc")
autoplot(cart_tune)
#cart_tune

# Finalize model
cart_best <- select_best(cart_tune, metric = "roc_auc")
cart_plot_best_mod <- select_by_one_std_err(cart_tune, metric = "roc_auc", desc(cost_complexity))

cart_final_workflow <-
  cart_workflow %>%
  finalize_workflow(cart_plot_best_mod)


cart_results <-
  cart_final_workflow %>%
  # rebuild the model using all training data (not resampled) and fit on test
  last_fit(split = analysis_split,
           metrics = metric_set(recall, precision, f_meas, accuracy, kap,
                                roc_auc, sens, spec))

# VIP
cart_vip <-
  cart_results %>%
  extract_fit_parsnip() %>%
  vip()

#saveRDS(cart_vip, "./data/cart_vip.rds")

# Look at the Metrics
cart_metrics <- cart_results %>%
  collect_metrics(summarize = TRUE)
cart_metrics

# Looking at results per person
cart_prediction <-
  cart_results %>%
  collect_predictions()
head(cart_prediction, n = 20)

# Confusion Matrix
cart_conf_mat <-
  cart_prediction %>%
  conf_mat(relapse, .pred_class) 


# Roc Curve
cart_roc_plot <-
  cart_prediction %>%
  roc_curve(relapse, .pred_0) %>%
  autoplot()

# Plotting prediction probabilities
cart_prediction_plot <-
  cart_prediction %>%
  ggplot() +
  geom_density(aes(x = .pred_0,
                   fill = relapse),
               alpha = 0.5) +
  labs(title = "True Relapse vs Prediction Probabilty",
       x = "Probabilty of...",
       y = "Density") +
  ggthemes::theme_few() +
  theme(legend.title=element_blank())

# Small decision tree 

# cart_trained <-
#   cart_results %>%
#   extract_fit_parsnip() # to create tree plot
#   cart_tree_fit <-  cart_trained$fit

cart_model_small <-
  decision_tree(cost_complexity = 0.01) %>%
  set_engine("rpart") %>%
  set_mode("classification")

cart_workflow_small <-
  workflow() %>%
  add_recipe(relapse_rec) %>%
  add_model(cart_model_small)

small_tree_results <- 
  cart_workflow_small %>%
  fit(data = analysis_train)

cart_trained <-
  small_tree_results %>%
  extract_fit_parsnip() # to create tree plot

cart_tree_fit <-  cart_trained$fit

# cart_small_tree <-
#   small_tree %>%
#   extract_fit_parsnip() %>%
#   extract_fit_engine() %>%
#   rpart.plot::rpart.plot(
#     roundint = FALSE,
#     tweak = 1,
#     clip.facs = TRUE
#   )


# Decision tree
# cart_results %>%
#   extract_fit_parsnip() %>%
#   extract_fit_engine() %>%
#   rpart.plot::rpart.plot(
#     roundint = FALSE,
#     tweak = 4,
#     clip.facs = TRUE
#   )
#
# #or
#
# cart_results %>%
#   extract_fit_parsnip() %>%
#   extract_fit_engine() %>%
#   treemisc::tree_diagram(box.palette = "BuOr")
#
#
# 


# Save results
if (!dir.exists(paste0(here::here(),"/data"))) {
  dir.create(paste0(here::here(),"/data"))
}
saveRDS(cart_conf_mat, file ="./data/cart_conf_mat.rds")
saveRDS(cart_metrics, file = "./data/cart_matrics.rds")
saveRDS(cart_vip, file = "./data/cart_vip.rds")
saveRDS(cart_roc_plot, file = "./data/cart_roc_plot.rds")
saveRDS(cart_prediction_plot, file = "./data/cart_prediction_plot.rds")
saveRDS(cart_tree_fit, file = "./data/cart_tree_fit.rds")