
# Load data
source("load.R")

# Load Preprocess data (contains train and test datasets)
source("preprocess.R")

# Define the model and the engine
glmnet_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# Create a workflow for logistic regression Lasso
glmnet_workflow <- workflow() %>%
  add_recipe(relapse_rec) %>%
  add_model(glmnet_spec)

# Define the grid for tuning the Lasso penalty
glmnet_grid <- data.frame(penalty = 10^seq(-6, -1, length.out = 20))

# Define the resampling method (5-fold cross-validation)
cv_folds <- vfold_cv(analysis_train, v = 5, strata = relapse)

# Tune the Lasso model using the defined grid and cross-validation
glmnet_tune <- tune_grid(glmnet_workflow, resamples = cv_folds, grid = glmnet_grid)

# Select the best model based on one standard error rule
favorite <- select_by_one_std_err(glmnet_tune, penalty, metric = "roc_auc")

# Print the selected model
favorite

# Update the workflow with the selected amount of shrinkage
final_wf <- finalize_workflow(glmnet_workflow, favorite)

# Fit the Lasso model using the training data
relapse_fit <- final_wf %>% fit(data = analysis_train)

# Print the fitted model
relapse_fit

## Review Fit on the Training Data

# Make predictions on the training data
relapselasso_pred <- predict(relapse_fit, analysis_train, type = "prob") %>%
  bind_cols(analysis_train %>% select(relapse))

# Print outcomes for the first 10 people
head(relapselasso_pred, n=10)

#plot the predicition

Pred_plot_lasso <- relapselasso_pred %>%
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



# Plot ROC curve for the Lasso model on the training data
roc_lasso_plot <- relapselasso_pred %>%
  roc_curve(truth = relapse, .pred_0) %>%
  autoplot()

# Calculate AUC for the Lasso model on the training data
relapselasso_pred %>%
  roc_auc(truth = relapse, .pred_0)

# Look at Model Metrics

# Evaluate the Lasso model on the test data
last_lasso_fit <- final_wf %>% last_fit(analysis_split)

# Collect model metrics
Lasso_metrics <- collect_metrics(last_lasso_fit, summarize = TRUE)

# Look at Variable Importance

# Use vip() function from the 'vip' package to calculate variable importance training
library(vip)

vip_lasso <- relapse_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

vip_lasso
## Review Fit on the Test Data

# Make predictions on the test data
relapselasso_pred <- predict(relapse_fit, analysis_test, type = "prob") %>%
  bind_cols(analysis_test %>% select(relapse))

# Plot ROC curve for the Lasso model on the test data
relapselasso_pred %>%
  roc_curve(truth = relapse, .pred_0) %>%
  autoplot()

# Calculate AUC for the Lasso model on the test data
relapselasso_pred %>%
  roc_auc(truth = relapse, .pred_0)

# Confusion Matrix Training

# Confusion Matrix
lasso_pred <- predict(relapse_fit, analysis_train, type = "class") %>%
  bind_cols(analysis_train %>% select(relapse))

lasso_conf_mat <- lasso_pred %>% 
  conf_mat(relapse, .pred_class)

lasso_conf_mat

vip_overall<- relapse_fit %>%
  extract_fit_parsnip() %>%
  vip::vi(lambda = favorite$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) |>
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  facet_wrap(~Sign, scales = "free_y") +
  theme(legend.position = "none")
# Create a directory for saving data if it doesn't exist
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}

# Save the coefficients to a file
saveRDS(vip_overall, "./data/vip_overall.rds")
saveRDS(Pred_plot_lasso, file = "./data/Pred_plot_lasso.rds")
saveRDS(roc_lasso_plot, file = "./data/roc_lasso_plot.rds")
saveRDS(Lasso_metrics, file = "./data/lasso_matrics.rds")
saveRDS(lasso_conf_mat, file = "./data/lasso_conf_mat.rds")
