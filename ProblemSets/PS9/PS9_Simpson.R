#Problem Set 9 

library(tidymodels)
library(glmnet)
library(tidyverse)
library(magrittr)
library(rsample)
library(recipes)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

set.seed(123456)

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between all predictors
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # create 6th degree polynomial terms of continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

dim(housing_train_prepped)
ncol(housing_train_prepped) - ncol(housing_train)



tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

rec_folds <- vfold_cv(housing_train_prepped, v = 6)

lambda_grid <- grid_regular(penalty(), levels = 50)

rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

best_rmse <- select_best(rec_res, metric = "rmse")

optimal_lambda <- best_rmse$penalty

final_lasso <- finalize_workflow(rec_wf, best_rmse)

fitted_model <- fit(final_lasso, data = housing_train_prepped)

in_sample_rmse <- fitted_model %>%
  predict(housing_train_prepped) %>%
  bind_cols(housing_train_y) %>%
  rmse(truth = medv, estimate = .pred) %>%
  pull(.estimate)

out_of_sample_rmse <- fitted_model %>%
  predict(housing_test_prepped) %>%
  bind_cols(housing_test_y) %>%
  rmse(truth = medv, estimate = .pred) %>%
  pull(.estimate)

cat("Optimal value of lambda:", optimal_lambda, "\n")
cat("In-sample RMSE:", in_sample_rmse, "\n")
cat("Out-of-sample RMSE:", out_of_sample_rmse, "\n")



tune_spec_ridge <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0 # 0 = ridge, 1 = lasso
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

rec_folds <- vfold_cv(housing_train_prepped, v = 6)

lambda_grid <- grid_regular(penalty(), levels = 50)

rec_wf_ridge <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_ridge)

rec_res_ridge <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

best_rmse_ridge <- select_best(rec_res_ridge, metric = "rmse")

optimal_lambda_ridge <- best_rmse_ridge$penalty

final_ridge <- finalize_workflow(rec_wf_ridge, best_rmse_ridge)

fitted_model_ridge <- fit(final_ridge, data = housing_train_prepped)

out_of_sample_rmse_ridge <- fitted_model_ridge %>%
  predict(housing_test_prepped) %>%
  bind_cols(housing_test_y) %>%
  rmse(truth = medv, estimate = .pred) %>%
  pull(.estimate)

cat("Optimal value of lambda for ridge regression:", optimal_lambda_ridge, "\n")
cat("Out-of-sample RMSE for ridge regression:", out_of_sample_rmse_ridge, "\n")
