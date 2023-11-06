# Load data and preprocessing functions
source("load.R")
source("preprocess.R")

# Define logistic regression model specification
logistic_spec <- logistic_reg()

# Create a workflow for logistic regression
logistic_workflow <- workflow() %>% 
  add_recipe(relapse_rec) %>% 
  add_model(logistic_spec)

# Fit the logistic regression model
logistic_fit <- logistic_workflow %>% 
  fit(data = analysis_train)

# Make predictions on the training data
relapse_pred <- predict(logistic_fit, analysis_train, type = "prob") %>% 
  bind_cols(analysis_train %>% select(relapse)) 

# Plot the ROC curve
logistic_ROC <- relapse_pred %>% 
  roc_curve(truth = relapse, .pred_0) %>% 
  autoplot()

# Calculate the ROC AUC
relapse_pred %>% 
  roc_auc(truth = relapse, .pred_0)

# Evaluate model metrics 
last_logistic_fit <- logistic_workflow %>% 
  last_fit(
    analysis_split, # use the test data
    metrics = metric_set(
      recall, precision, f_meas, accuracy, kap, roc_auc, sens, spec
    )
  )  

logistic_matrices <- collect_metrics(last_logistic_fit, summarize = TRUE)


# Confusion Matrix Training

logistic_conf_mat <- last_logistic_fit %>%
  collect_predictions() %>% 
  conf_mat(relapse, .pred_class)

# Create a directory for saving data if it doesn't exist
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}


# Prediction Probability Plot
logistic_pred_plot <- relapse_pred %>%
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



# Extract model coefficients and save them
logistic_betas <- logistic_fit |>
  tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) |>
  arrange(desc(abs(estimate))) |>
  select(term, estimate, conf.low, conf.high, std.error, p.value) |>
  mutate(across(estimate:std.error, ~round(.x, digits = 2))) |>
  mutate(p.value = scales::pvalue(p.value)) %>% 
  arrange(p.value)


# Save the coefficients to a file
saveRDS(logistic_betas, file = "./data/logistic_betas.rds")
saveRDS(logistic_pred_plot, file = "./data/logistic_pred_plot.rds")
saveRDS(logistic_ROC, file = "./data/logistic_ROC.rds")
saveRDS(logistic_matrices, file = "./data/logistic_matrices.rds")
saveRDS(logistic_conf_mat, file = "./data/logistic_conf_mat.rds")
