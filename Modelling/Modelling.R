install.packages('mlbench')
install.packages('tidymodels')
install.packages('tidyverse')
install.packages('workflows')
install.packages('tune')
install.packages('parsnip')

library(mlbench)
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)

# load the Pima Indians dataset from the mlbench dataset
data("PimaIndiansDiabetes")
diabetes_orig = PimaIndiansDiabetes

glimpse(diabetes_orig)

options(repr.plot.width=24, repr.plot.heigt=6)

diabetes_orig %>%
  pivot_longer(-diabetes) %>%
  ggplot() +
  geom_histogram(aes(x = value)) + facet_wrap(~name, scales = 'free')

diabetes_orig %>%
  mutate_at(vars(triceps, glucose, pressure, insulin, mass),
            function(.var) {
              if_else(condition = (.var == 0), # if true (i.e. the entry is 0)
                      true = as.numeric(NA), # replace the value with NA
                      false = .var # otherwise leave it as it is
              )
            }) -> diabetes_clean

diabetes_clean

na.omit()

#install.packages('corrplot')
#library(corrplot)
cor(mutate(diabetes_clean, diabetes = as.numeric(diabetes)) %>% na.omit())

corrplot(cor(mutate(diabetes_clean, diabetes = as.numeric(diabetes)) %>% na.omit()),
         method = "circle", 
         tl.pos = "d",
         #tl.cex = 1,
         #tl.col = '#85494A',
         tl.srt = 10) # lt, ld, td, d, n

set.seed(234589) # fixing random seed for replication
diabetes_split = initial_split(diabetes_clean,
                               prop = 3/4) # split the data into training (75 %) and testing (25 %)
diabetes_split

# this is possible but not necessary
diabetes_train = training(diabetes_split)
diabetes_test = testing(diabetes_split)

diabetes_train
diabetes_test

diabetes_cv = vfold_cv(diabetes_train, v = 5)
str(diabetes_cv)

# define the recipe
# which consists of the formula (outcome ~ predictors)
recipe(diabetes ~ .,
       data = diabetes_clean) %>%
  # and some pre-processing steps
  step_impute_knn(all_predictors()) %>%
  step_normalize(all_numeric()) -> diabetes_recipe

diabetes_recipe

# specify that the model is a random forest
rand_forest() %>%
  # specify that the 'mtry' and 'trees' parameter needs to be tuned - set_args(mtry = 4)
  # fixeds at non tuned
  set_args(mtry = tune(),
           trees = tune()) %>%
  # select the engine/package that underlies the model
  set_engine('ranger', importance = 'impurity') %>%
  # choose either the continuous regression or binary classification mode
  set_mode('classification') -> rf_model

# specifiy that the model is a logistic regression
logistic_reg() %>%
  # select the engine/package that underlines the model
  set_engine('glm') %>%
  # choose either the continuous regression or binary classification mode
  set_mode('classification') -> lr_model

# set the workflow
rf_workflow = workflow() %>%
  # add recipe
  add_recipe(diabetes_recipe) %>%
  # add model
  add_model(rf_model)

expand.grid(mtry = c(3, 6), trees = c(25, 50))

# specify which values you want to try
rf_grid = expand.grid(mtry = c(3, 5, 6), trees = c(10, 25, 50))
# extract results
rf_workflow %>%
  tune_grid(resamples = diabetes_cv, # CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metric we care about
            ) -> rf_tune_results

# print results
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
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split) -> rf_fit

rf_fit %>%
  select(-.workflow) %>%
  glimpse()

rf_fit$.predictions

test_performance = rf_fit %>% collect_metrics()
test_performance

# generate predictions from the test set
test_predictions = rf_fit %>% collect_predictions()
head(test_predictions)

# generate a confusion matrix
test_predictions %>%
  conf_mat(truth = diabetes, estimate = .pred_class)

test_predictions %>%
  ggplot() +
  geom_density(aes(x = .pred_pos, fill = diabetes),
               alpha = 0.5)

final_model = fit(rf_workflow, diabetes_clean)

final_model

# Variable importance

ranger_obj = extract_fit_parsnip(final_model)$fit # pull_workflow_fit() deprecated
ranger_obj

barplot(ranger_obj$variable.importance)

new_woman = tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, ~pedigree, ~age,
                    2, 250, 70, 31, 102, 28.2, 0.67, 47)

new_woman

predict(
  final_model,
  new_woman,
  type = 'prob'
)

predict(
  final_model,
  new_woman
)
