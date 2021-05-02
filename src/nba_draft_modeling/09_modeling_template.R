# https://supervised-ml-course.netlify.app/chapter1

# tidymodels steps 

# 1. ingest data 
# 2. visualize data 
# 3. split train/test data 
# 4. 

data("mtcars")

library(tidyverse)
library(tidymodels)

cars2018 <- read_csv("data/cars2018.csv")

# Deselect the 2 columns to create cars_vars
car_vars <- cars2018 %>%
  select(-model, -model_index)

# Fit a linear model
fit_all <- lm(mpg ~ ., data = car_vars)

# Print the summary of the model
summary(fit_all)

# splitting the dataset 
library(tidymodels)

car_split <- car_vars %>%
  initial_split(prop = 0.8,
                strata = aspiration)

car_train <- training(car_split)
car_test <- testing(car_split)

## a linear regression model specification
lm_mod <- linear_reg() %>%
  set_engine("lm")

lm_fit <- lm_mod %>%
  fit(log(mpg) ~ ., 
      data = car_train)

## a random forest model specification
rf_mod <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("randomForest")

fit_rf <- rf_mod %>%
  fit(log(mpg) ~ ., 
      data = car_train)

# Create the new columns
results <- car_train %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, car_train) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, car_train) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)

# then run this against the test data 
car_test <- readRDS("data/c1_test.rds")
fit_lm <- readRDS("data/c1_fit_lm.rds")
fit_rf <- readRDS("data/c1_fit_rf.rds")

# Create the new columns
results <- car_test %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, car_test) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, car_test) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)

## Create bootstrap resamples
car_boot <- bootstraps(car_train)

# Evaluate the models with bootstrap resampling
lm_res <- lm_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_res <- rf_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(rf_res)

results <-  bind_rows(lm_res %>%
                        collect_predictions() %>%
                        mutate(model = "lm"),
                      rf_res %>%
                        collect_predictions() %>%
                        mutate(model = "rf"))

glimpse(results)

results %>%
  ggplot(aes(`log(mpg)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model)





