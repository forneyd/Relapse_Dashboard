# Preprocess for modeling

# Set the random seed for replicability
set.seed(8701)

# Adding source code
source("load.R")

# Make Modeling Data
analysis_split <- analysis %>% 
  initial_split(strata = relapse)  # Split the data into training and testing sets

analysis_train <- training(analysis_split)
analysis_test <- testing(analysis_split)

# Make a Cross Validate Object
cv_folds <- vfold_cv(
  analysis_train,
  v = 5,  # Number of sets for cross-validation
  strata = relapse  # keep the percentage with relapse the same
)

# Double Check Modeling Data
analysis %>% 
  tabyl(relapse) %>% 
  adorn_pct_formatting(0) %>% 
  adorn_totals()

analysis_train %>% 
  tabyl(relapse) %>% 
  adorn_pct_formatting(0) %>% 
  adorn_totals()

analysis_test %>% 
  tabyl(relapse) %>% 
  adorn_pct_formatting(0) %>% 
  adorn_totals()

# Use the tidymodels Framework for Predictions

## Make a Recipe
relapse_rec <- 
  recipe(relapse ~ ., data = analysis_train) %>% 
  update_role(who, new_role = "ID") %>%  # Update the role of the 'who' variable to 'ID'
  step_nzv() %>%  # Remove near-zero variance predictors
  step_normalize(all_numeric_predictors()) %>%  # Normalize numeric predictors
  step_impute_median(all_numeric_predictors()) %>%  # Impute missing values in numeric predictors with median
  step_impute_mode(all_factor_predictors()) %>%  # Impute missing values in factor predictors with mode
  step_corr(all_numeric_predictors(), threshold = 0.7, method = "spearman") %>%  # Remove highly correlated predictors
  step_dummy(all_factor_predictors())

# Apply the recipe to the training data
processed <- prep(relapse_rec) %>% 
  bake(new_data = analysis_train)


# # Preprocess the training and testing data using the recipe
# data_train_preprocessed <- data_recipe %>% 
#   prep(data = data_train) %>% 
#   bake(data = data_train)
# 
# data_test_preprocessed <- data_recipe %>% 
#   prep(data = data_test) %>% 
#   bake(data = data_test)


# View summary statistics of the processed data
# skimr::skim(processed)