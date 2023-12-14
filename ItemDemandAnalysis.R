library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(modeltime) # Extensions of tidymodels to time series
library(timetk) # Some nice time series functions

# read in data ------------------------------------------------------------
setwd("/Users/justinross/Documents/BYU/stat348/ItemDemandChallenge")
train <- vroom("train.csv")
test <- vroom("test.csv")

# recipes -----------------------------------------------------------------

my_recipe <- recipe(sales ~ ., train) %>% 
  step_date(date, features="month") %>% 
  step_date(date, features="year") %>% 
  step_date(date, features = "doy") %>%
  step_date(date, features="dow") %>% 
  step_rm(date) %>% 
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) #%>% 
  #step_lag(sales, lag = 7) %>% 


prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = storeItem) # Make sure recipe work on train
bake(prepped_recipe, new_data = test) # Make sure recipe works on test


# random forest -----------------------------------------------------------

rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>% # 500 or 1000
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod)

tuning_grid <- grid_regular(mtry(range = c(1, (ncol(storeItem3)-1))),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(storeItem3, v = 5, repeats = 5)

rf_CV_results <- rf_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

bestTune <- rf_CV_results %>%
  select_best("smape")

collect_metrics(rf_CV_results) %>% 
  filter(mtry == 3, min_n == 21) %>% 
  pull(mean)
# 13.06806


# exponential smoothing ---------------------------------------------------

storeItem1 <- train %>% filter(store==1, item==26)
storeItem2 <- train %>% filter(store==6, item==42)

cv_split1 <- time_series_split(storeItem1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)

cv_split1 %>%
  tk_time_series_cv_plan() %>% # Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split2 %>%
  tk_time_series_cv_plan() %>% # Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model1 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split1))

es_model2 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split2))

## Cross-validate to tune model
cv_results1 <- modeltime_calibrate(es_model1,
                                  new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(es_model2,
                                   new_data = testing(cv_split2))

## Visualize CV results
p1 <- cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = storeItem1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p2 <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive = FALSE)

## Evaluate the accuracy
cv_results1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

cv_results2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

# Refit to all data then forecast
es_fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItem1)

es_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItem2)

es_preds1 <- es_fullfit1 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

es_preds2 <- es_fullfit2 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

p3 <- es_fullfit1 %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

p4 <- es_fullfit2 %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

submission <- plotly::subplot(p1,p2,p3,p4, nrows = 2)

ggsave("forecast_plot.png", submission)


# ARIMA -------------------------------------------------------------------

storeItem1 <- train %>%
  filter(store == 3, item == 10)
storeItem2 <- train %>%
  filter(store == 5, item == 37)

storeItem_train1 <- train %>%
  filter(store == 3, item == 10)
storeItemtest1 <- test %>%
  filter(store == 3, item == 10)

storeItem_train2 <- train %>%
  filter(store == 5, item == 37)
storeItemtest2 <- test %>%
  filter(store == 5, item == 37)

my_recipe <- recipe(sales ~ ., storeItem_train1) %>% 
  step_date(date, features="month") %>% 
  step_date(date, features="year") %>% 
  step_date(date, features = "doy") %>%
  step_date(date, features="dow") %>% 
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

cv_split1 <- time_series_split(storeItem_train1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItem_train2, assess="3 months", cumulative = TRUE)

arima_model <- arima_reg(seasonal_period=365,
                           non_seasonal_ar=5, # default max p to tune
                           non_seasonal_ma=5, # default max q to tune
                           seasonal_ar=2, # default max P to tune
                           seasonal_ma=2, #default max Q to tune
                           non_seasonal_differences=2, # default max d to tune
                           seasonal_differences=2) %>%  #default max D to tune
  set_engine("auto_arima")

arima_wf1 <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split1))

arima_wf2 <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))

## Cross-validate to tune model
cv_results1 <- modeltime_calibrate(arima_wf1,
                                   new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(cv_split2))

## Visualize CV results
p1 <- cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = storeItem1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p2 <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive = FALSE)

## refit to data
fullfit1 <- cv_results1 %>%
  modeltime_refit(data=storeItem_train1)

fullfit2 <- cv_results2 %>%
  modeltime_refit(data=storeItem_train2)


p3 <- fullfit1 %>%
  modeltime_forecast(new_data = storeItemtest1,
                     actual_data = storeItem_train1) %>%
  plot_modeltime_forecast(.interactive=F)

p4 <- fullfit2 %>%
  modeltime_forecast(new_data = storeItemtest2,
                     actual_data = storeItem_train2) %>%
  plot_modeltime_forecast(.interactive=F)

submission <- plotly::subplot(p1,p2,p3,p4, nrows = 2)


# Facebook's Prophet Model ------------------------------------------------

storeItem1 <- train %>% filter(store==1, item==26)
storeItem2 <- train %>% filter(store==6, item==42)

cv_split1 <- time_series_split(storeItem1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)

prophet_model1 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split1))

prophet_model2 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split2))

## Calibrate (i.e. tune) workflow
cv_results1 <- modeltime_calibrate(prophet_model1,
                                   new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(prophet_model2,
                                   new_data = testing(cv_split2))


## Visualize & Evaluate CV accuracy
p1 <- cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = storeItem1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

p2 <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive = FALSE)


## Refit best model to entire data and predict

FPM_fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItem1)

FPM_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItem2)

FPM_preds1 <- FPM_fullfit1 %>%
  modeltime_forecast(h = "3 months", new_data = storeItem1) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

FPM_preds2 <- FPM_fullfit2 %>%
  modeltime_forecast(h = "3 months", new_data = storeItem2) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)


p3 <- FPM_fullfit1 %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

p4 <- FPM_fullfit2 %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

submission <- plotly::subplot(p1,p2,p3,p4, nrows = 2)




