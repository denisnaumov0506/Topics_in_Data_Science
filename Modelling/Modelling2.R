library(mlbench)
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(parsnip)

#install.packages('fastDummies')
library(fastDummies)

space_data = read_csv('https://raw.githubusercontent.com/NikoStein/TDS_data/main/melon_tusk.csv')

# Insert your code here
head(space_data, n=10)

# Remove identifying variables, namely PassengerId and Name
space_data %>%
  select(-c('PassengerId', 'Name')) -> new_df

# Remove NA Values from dataset
#na.omit(new_df) -> space_no_na

# Turn all categorical data columns to dummy variables
space_no_categorical_values = dummy_cols(new_df,
                                         select_columns = c('HomePlanet', 'Cabin', 'Destination')) %>%
  select(-c('HomePlanet', 'Cabin', 'Destination'))

# Turn all true/false values to 1s and 0s
space_no_categorical_values %>%
  mutate_at(vars('CryoSleep', 'VIP'),
            function(.var) {
              if_else(condition = (.var == TRUE),
                      true = as.numeric(TRUE),
                      false = as.numeric(FALSE))
            }) -> space_clean

# Turn outcome variablet to factor
space_clean$Refunded = as.factor(space_clean$Refunded)

space_clean

space_split = initial_split(space_clean, prop= 3/4)

space_split

space_train = training(space_split)
space_test = testing(space_split)

# Insert your code here
space_cv = vfold_cv(space_train, v=5)

recipe(Refunded ~ .,
       data = space_clean) %>%
  # pre-processing
  step_impute_knn(all_predictors()) %>%
  step_normalize(all_numeric()) -> space_recipe

space_recipe

# Insert your code here
rand_forest() %>%
  set_args(mtry = tune(),
           trees = tune()) %>%
  set_engine('ranger', importance = 'impurity') %>%
  set_mode('classification') -> rf_model

rf_workflow = workflow() %>%
  add_recipe(space_recipe) %>%
  add_model(rf_model)

rf_grid = expand.grid(mtry = c(3, 5, 6), trees = c(10, 25, 50))

# Insert your code here
rf_workflow %>%
  tune_grid(resamples = space_cv,
            grid = rf_grid,
            metrics = metric_set(accuracy, roc_auc)) -> rf_tune_results

rf_tune_results %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  arrange(.metric, -mean)

rf_tune_results %>%
  select_best(metric = "accuracy") -> param_final

param_final

rf_workflow %>%
  finalize_workflow(param_final) -> rf_workflow

rf_workflow %>%
  last_fit(space_split) -> rf_fit

rf_fit %>%
  select(-.workflow) %>%
  glimpse()

rf_fit$.predictions

test_performance = rf_fit %>% collect_metrics()
test_performance

