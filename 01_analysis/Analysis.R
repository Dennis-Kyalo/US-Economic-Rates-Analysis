# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core 
library(tidyquant)
library(tidyverse)
library(timetk)

# 1.0 Load the data ----
# Load the economics data
read_rds(file = "data/economics_data.rds")
economics_data


# * Parallel Processing ----
registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))

# * Visualize the data ----
economics_data %>% 
    pivot_longer(inflation_rate:bond_rate, names_to = "name", values_to = "rate") %>% 
    mutate(name = forcats::as_factor(name)) %>% 
    group_by(name) %>%
    plot_time_series(.date_var = date, .value = rate, .facet_ncol = 2, .smooth = FALSE)

# 2.0 PREPARING THE DATA ----
# * Full Dataset ----
full_data_tbl <- economics_data %>% 
    pivot_longer(inflation_rate:bond_rate, names_to = "name", values_to = "rate") %>% 
    
    # Group-Wise Feature Transformations
    # Extending the data 24 months to the future
    group_by(name) %>%
    future_frame(date, .length_out = 12, .bind_data = TRUE) %>%
    ungroup() %>%
    
 
        # Global Features / Transformations / Joins
    mutate(name = as_factor(name)) %>% 
    group_by(name) %>% 
    group_split() %>%
    map(.f = function(df){
        df %>% 
            arrange(date) %>% 
            tk_augment_fourier(date, .periods = c(3, 6, 12)) %>% 
            tk_augment_lags(rate, .lags      = 1) %>% 
            tk_augment_slidify(
                rate_lag1,
                .f       = ~ mean(.x, na.rm = TRUE),
                .period  = c(3, 6, 12),
                .partial = TRUE,
                .align   = "center"
            )
    }) %>% 
    bind_rows() 

full_data_tbl %>% arrange(rate)


# * Future Data ----
future_tbl <- full_data_tbl %>%
    filter(is.na(rate)) %>%
    
    mutate(
        across(.cols = contains("_lag"), 
               .fns  = function(x) ifelse(is.nan(x), NA, x))
    ) %>% 
    
    mutate(across(
        .cols = contains("rate"),
        .fns  = function(x)
            ifelse(is.nan(x), NA, x)
    )) %>%
    mutate(across(.cols = contains("_lag"), .fns = ~ replace_na(.x, 0)))
    

future_tbl 


# * Economics data prepared ----
economics_features_tbl <- full_data_tbl %>% 
    filter(!is.na(rate)) %>% 
    drop_na()

economics_features_tbl

# * Time Split ----
# Splitting the data into train and test set
splits <- economics_features_tbl %>% 
    time_series_split(date_var = date, assess = 120, cumulative = TRUE)

    
splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, rate)


# * Clean Training Set ----
# - With Panel Data, need to do this outside of a recipe
# - Transformation happens by group

train_economic_features_tbl <- training(splits)

train_economic_features_tbl %>%
    group_by(name) %>%
    plot_time_series(date, rate, .facet_ncol = 2, .smooth = FALSE)


# * Recipe Specification ----

train_economic_features_tbl

recipe_spec <- recipe(rate ~ ., data = train_economic_features_tbl) %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(date_index.num, date_year) %>%
    step_other(name) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()


# 3.0 MODELS ----

# * ARIMA ----

arima_model <- function(title = "inflation_rate") {
    
    set.seed(123)
    model_fit_auto_arima <- arima_reg() %>%
        set_engine("auto_arima") %>%
        fit(
            rate ~ date,
            data = train_economic_features_tbl %>% filter(name == title)
        )
    
    # * Calibrate ----
    
    calibration_tbl <- modeltime_table(
        model_fit_auto_arima
    ) %>%
        modeltime_calibrate(testing(splits) %>% filter(name == title))
    
    # * Refit ----
    
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(economics_features_tbl %>% filter(name == title))
    
    refitted_tbl <- refit_tbl %>%
        modeltime_forecast(
            new_data = future_tbl %>% filter(name == title),
            actual_data = economics_features_tbl %>% filter(name == title)
        ) %>% add_column(name = as.character(str_glue({title})))
    
    return(refitted_tbl)
    
}

# Accuracy Function

arima_accuracy <- function(title = "inflation_rate") {
    
    set.seed(123)
    model_fit_auto_arima <- arima_reg() %>%
        set_engine("auto_arima") %>%
        fit(
            rate ~ date,
            data = train_economic_features_tbl %>% filter(name == title)
        )
    
    # * Calibrate ----
    
    calibration_tbl <- modeltime_table(
        model_fit_auto_arima
    ) %>%
        modeltime_calibrate(testing(splits) %>% filter(name == title))
    
    calibration_tbl %>% modeltime_accuracy()
    
}    

inflation_arima_accuracy_tbl <- arima_accuracy(title = "inflation_rate")
tbill_arima_accuracy_tbl     <- arima_accuracy(title = "tbill_rate")
bank_arima_accuracy_tbl      <- arima_accuracy(title = "bank_rate")
mortgage_arima_accuracy_tbl  <- arima_accuracy(title = "mortgage_rate")
bond_arima_accuracy_tbl      <- arima_accuracy(title = "bond_rate")


inflation_arima_tbl <- arima_model(title = "inflation_rate")
tbill_arima_tbl     <- arima_model(title = "tbill_rate")
bank_arima_tbl      <- arima_model(title = "bank_rate")
mortgage_arima_tbl  <- arima_model(title = "mortgage_rate")
bond_arima_tbl      <- arima_model(title = "bond_rate")

g <- bond_arima_tbl %>% 
    ggplot(aes(x = .index,   y = .value)) +
    geom_line(aes(color      = .model_desc)) +
    scale_x_date(date_breaks = "8 years", 
                 date_labels = "%Y") +
    
    scale_color_tq() +
    theme_tq() 

plotly::ggplotly(g)

# * PROPHET ----

wflw_fit_prophet <- workflow() %>%
    add_model(spec = prophet_reg() %>% set_engine("prophet")) %>%
    add_recipe(recipe_spec) %>%
    fit(train_economic_features_tbl)

# * XGBOOST ----

wflw_fit_xgboost <- workflow() %>%
    add_model(spec = boost_tree(mode = "regression") %>%
                  set_engine("xgboost")) %>%
    add_recipe(recipe_spec %>% step_rm(date)) %>%
    fit(train_economic_features_tbl)

# * PROPHET BOOST ----

wflw_fit_prophet_boost <- workflow() %>%
    add_model(
        spec = prophet_boost(
            seasonality_daily  = FALSE,
            seasonality_weekly = FALSE,
            seasonality_yearly = FALSE
        ) %>%
            set_engine("prophet_xgboost")
    ) %>%
    add_recipe(recipe_spec) %>%
    fit(train_economic_features_tbl)


# * SVM ----

wflw_fit_svm <- workflow() %>%
    add_model(spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")) %>%
    add_recipe(recipe = recipe_spec %>% step_rm(date)) %>%
    fit(train_economic_features_tbl)


# * RANDOM FOREST ----

wflw_fit_rf <- workflow() %>%
    add_model(spec = rand_forest(mode = "regression") %>% set_engine("ranger")) %>%
    add_recipe(recipe = recipe_spec %>% step_rm(date)) %>%
    fit(train_economic_features_tbl)


# * ACCURACY CHECK ----

submodels_1_tbl <- modeltime_table(
    wflw_fit_prophet,
    wflw_fit_xgboost,
    wflw_fit_prophet_boost,
    wflw_fit_svm,
    wflw_fit_rf,
    wflw_fit_mars
)


submodels_1_tbl %>% 
    modeltime_accuracy(testing(splits)) %>%
    arrange(rmse) 


# 4.0 HYPER PARAMETER TUNING ----

set.seed(123)
resamples_kfold <- train_economic_features_tbl %>% vfold_cv(v = 5)


resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(.date_var = date, .value = rate)


# * XGBOOST TUNE ----

# ** Tunable Specification

model_spec_xgboost_tune <- boost_tree(
    mode            = "regression", 
    mtry            = tune(),
    trees           = tune(),
    min_n           = tune(),
    tree_depth      = tune(),
    learn_rate      = tune(),
    loss_reduction  = tune()
) %>% 
    set_engine("xgboost")


wflw_spec_xgboost_tune <- workflow() %>%
    add_model(model_spec_xgboost_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))



tic()
set.seed(123)
tune_results_xgboost <- wflw_spec_xgboost_tune %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_xgboost_tune) %>%
            update(
                learn_rate = learn_rate(range = c(0.001, 0.400), trans = NULL)
            ),
        grid = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

# ** Results

tune_results_xgboost %>% show_best("rmse", n = Inf)

# ** Finalize

wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>%
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>%
    fit(train_economic_features_tbl)


# * RANGER TUNE ----

# ** Tunable Specification

model_spec_rf_tune <- rand_forest(
    mode    = "regression",
    mtry    = tune(),
    trees   = tune(),
    min_n   = tune()
) %>% 
    set_engine("ranger")


wflw_spec_rf_tune <- workflow() %>%
    add_model(model_spec_rf_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning

tic()
set.seed(123)
tune_results_rf <- wflw_spec_rf_tune %>%
    tune_grid(
        resamples = resamples_kfold,
        grid      = 5,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

# ** Results

tune_results_rf %>% show_best("rmse", n = Inf)

# ** Finalize

wflw_fit_rf_tuned <- wflw_spec_rf_tune %>%
    finalize_workflow(select_best(tune_results_rf, "rmse")) %>%
    fit(train_economic_features_tbl)


# * EARTH TUNE ----

# ** Tunable Specification

model_spec_earth_tune <- mars(
    mode        = "regression",
    num_terms   = tune(),
    prod_degree = tune()
) %>%
    set_engine("earth")

wflw_spec_earth_tune <- workflow() %>%
    add_model(model_spec_earth_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning

tic()
set.seed(123)
tune_results_earth <- wflw_spec_earth_tune %>%
    tune_grid(
        resamples = resamples_kfold, 
        grid      = 10,
        control   = control_grid(allow_par = TRUE, verbose = TRUE)
    )
toc()


# ** Results
tune_results_earth %>% show_best("rmse")


# ** Finalize
wflw_fit_earth_tuned <- wflw_spec_earth_tune %>%
    finalize_workflow(tune_results_earth %>% select_best("rmse")) %>%
    fit(train_economic_features_tbl)



# 5.0 EVALUATE PANEL FORECASTS  -----

# * Model Table ----

submodels_2_tbl <- modeltime_table(
    wflw_fit_xgboost_tuned,
    wflw_fit_rf_tuned,
    wflw_fit_earth_tuned
) %>%
    update_model_description(1, "XGBOOST - Tuned") %>%
    update_model_description(2, "RANGER - Tuned") %>%
    update_model_description(3, "EARTH - Tuned") %>% 
    combine_modeltime_tables(submodels_1_tbl)


# * Calibration ----
calibration_tbl <- submodels_1_tbl %>%
    modeltime_calibrate(testing(splits))

# * Accuracy ----
accuracy_tbl <- calibration_tbl %>% 
    modeltime_accuracy()

# * Forecast Test ----

calibration_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = train_economic_features_tbl,
        keep_data   = TRUE 
    ) %>%
    
    group_by(name) %>%
    plot_modeltime_forecast(
        .facet_ncol         = 2, 
        .conf_interval_show = FALSE,
        .interactive        = TRUE
    )


# * Turn OFF Parallel Backend
plan(sequential)


# 6.0 FORECASTING ----
forecast_future_tbl <- submodels_1_tbl   %>% 
    modeltime_refit(data = economics_features_tbl) %>% 
    modeltime_forecast(new_data    = future_tbl, 
                       actual_data = economics_features_tbl, 
                       keep_data   = TRUE) 

forecast_future_tbl %>% group_by(name) %>% 
plot_modeltime_forecast(.conf_interval_show = FALSE, .facet_ncol = 2)


# * Final Full Table ----

final_tbl <- forecast_future_tbl %>%
    full_join(inflation_arima_tbl) %>% 
    full_join(tbill_arima_tbl) %>% 
    full_join(bank_arima_tbl) %>% 
    full_join(mortgage_arima_tbl) %>% 
    full_join(bond_arima_tbl) %>% 
    select(.model_id:name) %>% 
    rename(date = .index, rate = .value) %>% 
    relocate(name, .after = .key)

# Updating Names
final_tbl <- final_tbl %>% 
    mutate(.model_desc = case_when(
        .model_desc == "UPDATE: ARIMA(2,1,1)(0,0,2)[12]" ~ "Inflation: UPDATE: ARIMA(2,1,1)(0,0,2)[12]",
        .model_desc == "UPDATE: ARIMA(5,1,4)(2,0,1)[12]" ~ "Tbill: UPDATE: ARIMA(5,1,4)(2,0,1)[12]",
        .model_desc == "UPDATE: ARIMA(2,1,3)(1,0,1)[12]" ~ "Bank: UPDATE: ARIMA(2,1,3)(1,0,1)[12]",
        .model_desc == "UPDATE: ARIMA(0,1,1)"            ~ "Mortgage: UPDATE: ARIMA(0,1,1)",
        .model_desc == "UPDATE: ARIMA(0,1,3)(0,0,1)[12]" ~ "Bond: UPDATE: ARIMA(0,1,3)(0,0,1)[12]",
        TRUE ~ .model_desc
    )
)

# * Final Accuracy Table

ml_accuracy_tbl <- accuracy_tbl 

arima_accuracy_tbl <- inflation_arima_accuracy_tbl %>% 
    full_join(tbill_arima_accuracy_tbl) %>% 
    full_join(bank_arima_accuracy_tbl) %>% 
    full_join(mortgage_arima_accuracy_tbl) %>% 
    full_join(bond_arima_accuracy_tbl) 

# Updating names    
arima_accuracy_tbl <- arima_accuracy_tbl %>%     
    mutate(.model_desc = case_when(
        .model_desc == "ARIMA(1,1,3)(0,0,2)[12]"            ~ "Inflation: ARIMA(1,1,3)(0,0,2)[12]",
        .model_desc == "ARIMA(5,1,4)(2,0,2)[12] WITH DRIFT" ~ "Tbill: ARIMA(5,1,4)(2,0,2)[12] WITH DRIFT",
        .model_desc == "ARIMA(2,1,3)(1,0,1)[12]"            ~ "Bank: ARIMA(2,1,3)(1,0,1)[12]",
        .model_desc == "ARIMA(0,1,1)"                       ~ "Mortgage: ARIMA(0,1,1)",
        .model_desc == "ARIMA(0,1,3)(0,0,1)[12]"            ~ "Bond: ARIMA(0,1,3)(0,0,1)[12]", 
        TRUE ~ .model_desc 
    )
  ) %>% 
    separate(col = .model_desc, into = "name", sep = c(": "), remove = FALSE)

arima_accuracy_tbl


































