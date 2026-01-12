library(tidymodels)
library(mlbench)

data(PimaIndiansDiabetes)

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


set.seed(123)

diabetes_split <- initial_split(
  diabetes_df,
  prop = 0.75,
  strata = diabetes
)

diabetes_train <- training(diabetes_split)
diabetes_test  <- testing(diabetes_split)


diabetes_recipe <- recipe(diabetes ~ ., data = diabetes_train) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


log_reg_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")


diabetes_workflow <- workflow() %>%
  add_recipe(diabetes_recipe) %>%
  add_model(log_reg_spec)


diabetes_fit <- fit(
  diabetes_workflow,
  data = diabetes_train
)


diabetes_pred <- predict(
  diabetes_fit,
  diabetes_test,
  type = "prob"
) %>%
  bind_cols(
    predict(diabetes_fit, diabetes_test, type = "class"),
    diabetes_test
  )

metrics(diabetes_pred, truth = diabetes, estimate = .pred_class)

roc_auc(diabetes_pred, truth = diabetes, .pred_pos)


saveRDS(diabetes_fit, "models/diabetes_tidymodels.rds")
