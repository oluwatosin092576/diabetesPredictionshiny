# ------------------------------
# 1. Load packages
# ------------------------------
library(tidymodels)
library(mlbench)
library(lightgbm)
library(xgboost)
library(dials)

set.seed(123)

# ------------------------------
# 2. Load and prepare data
# ------------------------------
data("PimaIndiansDiabetes")

diabetes_df <- PimaIndiansDiabetes %>%
  rename(
    blood_pressure = pressure,
    skin_thickness = triceps,
    bmi = mass,
    diabetes_pedigree = pedigree
  ) %>%
  mutate(
    diabetes = factor(diabetes, levels = c("neg", "pos"))
  )

# ------------------------------
# 3. Train/Test split
# ------------------------------
split <- initial_split(diabetes_df, prop = 0.8, strata = diabetes)
train_data <- training(split)
test_data  <- testing(split)

# ------------------------------
# 4. 5-fold cross-validation
# ------------------------------
cv_folds <- vfold_cv(
  train_data,
  v = 5,
  strata = diabetes
)

# ------------------------------
# 5. Recipe (preprocessing)
# ------------------------------
diabetes_recipe <- recipe(diabetes ~ ., data = train_data) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# ------------------------------
# 6. Model specifications
# ------------------------------

# Logistic Regression
glm_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# Random Forest
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# XGBoost
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# LightGBM
lgbm_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("classification")

# ------------------------------
# 7. Workflows
# ------------------------------
glm_wf  <- workflow() %>% add_recipe(diabetes_recipe) %>% add_model(glm_spec)
rf_wf   <- workflow() %>% add_recipe(diabetes_recipe) %>% add_model(rf_spec)
xgb_wf  <- workflow() %>% add_recipe(diabetes_recipe) %>% add_model(xgb_spec)
lgbm_wf <- workflow() %>% add_recipe(diabetes_recipe) %>% add_model(lgbm_spec)

# ------------------------------
# 8. Metrics
# ------------------------------
metrics <- metric_set(roc_auc, accuracy, sens, spec)

# ------------------------------
# 9. Hyperparameter search spaces
# ------------------------------

glm_params <- parameters(
  penalty(range = c(-4, 0)),  # log10 scale: 1e-4 → 1
  mixture(range = c(0, 1))    # ridge → lasso
)

rf_params <- parameters(
  mtry(range = c(2, 7)),
  min_n(range = c(2, 20))
)

xgb_params <- parameters(
  trees(range = c(200, 1000)),
  tree_depth(range = c(2, 8)),
  learn_rate(range = c(-4, -1)),   # log10 scale
  loss_reduction(range = c(0, 10)),
  min_n(range = c(2, 20))
)

lgbm_params <- parameters(
  trees(range = c(200, 1000)),
  tree_depth(range = c(2, 8)),
  learn_rate(range = c(-4, -1)),   # log10 scale
  min_n(range = c(2, 20))
)

# ------------------------------
# 10. Tuning grids (Latin Hypercube)
# ------------------------------
glm_grid <- grid_latin_hypercube(glm_params, size=30)
rf_grid   <- grid_latin_hypercube(rf_params, size = 30)
xgb_grid  <- grid_latin_hypercube(xgb_params, size = 30)
lgbm_grid <- grid_latin_hypercube(lgbm_params, size = 30)

ctrl <- control_grid(
  save_pred = TRUE,
  verbose = TRUE,
  allow_par = TRUE
)

# ------------------------------
# 11. Tune models
# ------------------------------
rf_tuned <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metrics,
  control = ctrl
)

xgb_tuned <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metrics,
  control = ctrl
)

lgbm_tuned <- tune_grid(
  lgbm_wf,
  resamples = cv_folds,
  grid = lgbm_grid,
  metrics = metrics,
  control = ctrl
)

# ------------------------------
# 12. Fit GLM (no tuning)
# ------------------------------
glm_tuned <- tune_grid(
  glm_wf,
  resamples = cv_folds,
  grid = glm_grid,
  metrics = metrics
)

# ------------------------------
# 13. Compare models (ROC AUC)
# ------------------------------
results <- bind_rows(
  collect_metrics(glm_fit)   %>% mutate(model = "GLM"),
  collect_metrics(rf_tuned)  %>% mutate(model = "Random Forest"),
  collect_metrics(xgb_tuned) %>% mutate(model = "XGBoost"),
  collect_metrics(lgbm_tuned)%>% mutate(model = "LightGBM")
)

best_model <- results %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(mean)) %>%
  slice(1)

print(best_model)

# ------------------------------
# 14. Finalize best workflow
# ------------------------------
best_workflow <- switch(
  best_model$model,
  "GLM" = glm_wf,
  "Random Forest" = finalize_workflow(
    rf_wf,
    select_best(rf_tuned, metric = "roc_auc")
  ),
  "XGBoost" = finalize_workflow(
    xgb_wf,
    select_best(xgb_tuned, metric = "roc_auc")
  ),
  "LightGBM" = finalize_workflow(
    lgbm_wf,
    select_best(lgbm_tuned, metric = "roc_auc")
  ),
  stop("Unknown model type: ", best_model$model)
)


final_fit <- fit(best_workflow, train_data)

# ------------------------------
# 15. Save model for Shiny app
# ------------------------------
saveRDS(final_fit, "models/diabetes_tidymodels.rds")

