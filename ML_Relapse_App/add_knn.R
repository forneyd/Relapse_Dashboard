# Load data and preprocessing functions
source("load.R")
source("preprocess.R")

# Make a Recipe
relapse_rec_knn <- relapse_rec

# Specify how to predict using KNN
kknn_spec <- nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 

# Create a workflow for KNN
kknn_workflow <- workflow() %>% 
  add_recipe(relapse_rec_knn) %>% 
  add_model(kknn_spec) 

# Register parallel processing
registerDoParallel()

# Tune KNN hyperparameters using grid search
kknn_tune <- kknn_workflow %>% 
  tune_grid(
    resamples = cv_folds, 
    grid = data.frame(neighbors = seq(3, 53, by = 5)),
    metrics = metric_set(
      roc_auc, recall, precision, f_meas, accuracy, kap, sens, spec
    )
  )

# Stop implicit cluster for parallel processing
stopImplicitCluster()

# Set parameter limits for a better search
knn_parameters <- kknn_workflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    neighbors = neighbors(c(3, 53)) # limits on the number of neighbors to check
  )

# Tune with a Bayesian optimization algorithm
suppressMessages(conflict_prefer("spec", "yardstick"))

ctrl <- control_bayes(
  verbose = TRUE, # show progress as it searches
  save_pred = TRUE # save the predicted probabilities
)

registerDoParallel() # use multiple cores for faster processing

kknn_tune <- tune_bayes(
  kknn_workflow,
  resamples = cv_folds, # cross-validation details
  initial = 10, # number of initial tries
  iter = 20, # max number of search iterations
  param_info = knn_parameters, # object with limits
  metrics = metric_set(roc_auc, accuracy, kap), # save many evaluation metrics
  control = ctrl
)

# Update the workflow with the best hyperparameters
kknn_final_workflow <- kknn_workflow %>%
  finalize_workflow(
    select_best(kknn_tune, "roc_auc")
  )

# Fit the model
replace_fit <- kknn_final_workflow %>% 
  fit(data = analysis_train)

# Predict on the training data 
replace_pred <- predict(
  replace_fit, analysis_train, type = "prob"
) %>% 
  bind_cols(
    analysis_train %>% 
      select(relapse)
  ) 

# Prediction Probability Plot
knn_pred_plot <- replace_pred %>%
  ggplot() +
  geom_density(
    aes(x = .pred_0, fill = relapse),
    alpha = 0.5
  ) +
  labs(
    title = "True Relapse vs Prediction Probabilty",
    x = "Probability Of Relapse",
    y = "Density"
  ) +
  ggthemes::theme_few() +
  theme(
    legend.title = element_blank()
  )

# Generate ROC curve plot
knn_roc_plot <- replace_pred %>% 
  roc_curve(truth = relapse, .pred_0) %>% 
  autoplot()

# Evaluate model metrics 
last_knn_fit <- kknn_final_workflow %>% 
  last_fit(
    analysis_split, # use the test data
    metrics = metric_set(
      recall, precision, f_meas, accuracy, kap, roc_auc, sens, spec
    )
  )  

knn_matrices <- collect_metrics(last_knn_fit, summarize = TRUE)

# Generate confusion matrix
knn_conf_mat <- last_knn_fit %>%
  collect_predictions() %>% 
  conf_mat(relapse, .pred_class)

# Save results
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}

saveRDS(knn_matrices, file = here::here("data", "knn_matrices.rds"))
saveRDS(knn_conf_mat, file = here::here("data", "knn_conf_mat.rds"))
saveRDS(knn_pred_plot, file = here::here("data", "knn_pred_plot.rds"))
saveRDS(knn_roc_plot, file = here::here("data", "knn_roc_plot.rds"))


# This script performs K-nearest neighbors (KNN) classification for relapse prediction.
# It loads the necessary data and preprocessing functions, creates a KNN workflow, and tunes the hyperparameters using grid search and Bayesian optimization.
# The best hyperparameters are selected based on the ROC AUC metric.
# The model is then fitted, and predictions are made on the training data.
# Visualization plots, such as the prediction probability plot and ROC curve plot, are generated.
# Finally, the model is evaluated using various metrics, including recall, precision, F-measure, accuracy, kappa, ROC AUC, sensitivity, and specificity.
# The evaluation results, including the confusion matrix, are saved in the "data" directory.
