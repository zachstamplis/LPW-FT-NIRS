# Helper packages
library(dplyr)    # for general data wrangling needs

# Modeling packages
library(gbm)      # for original implementation of regular and stochastic GBMs
library(h2o)      # for a java-based implementation of GBM variants
library(xgboost)  # for fitting extreme gradient boosting

# Modeling packages
library(ranger)   # a c++ implementation of random forest 
library(h2o)      # a java-based implementation of random forest


df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
df <- cbind(read_age = df$read_age,df[,21:ncol(df)])
names(df) <- make.names(names(df))

# number of features (explanatory variables?)
n_features <- 933

# create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# train a default random forest model
lpw_rf <- ranger(
   
  data = df,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  num.trees = n_features * 10,
  
)

for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = read_age ~ .,
    data            = df, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

BEST10 <- hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)



h2o.no_progress()
h2o.init(max_mem_size = "5g")

# convert training data to h2o object
train_h2o <- as.h2o(df)

# set the response column to Sale_Price
response <- "read_age"

# set the predictor names
predictors <- setdiff(colnames(df), response)

h2o_rf1 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = train_h2o, 
  ntrees = n_features * 10,
  seed = 123
)

h2o_rf1














# perform grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = response, 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10,
  seed = 123,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,           # stop if last 10 trees added 
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)













# 1. Initialize H2O
library(h2o)
h2o.init()

# 2. Convert data to h2o format
nirs_data_h2o <- as.h2o(df)

# 3. Define predictors and response
predictors <- colnames(df)[2:934]  # NIRS wavelengths
response <- "read_age"  # Your age column name

# 4. Set up cross-validation
model <- h2o.randomForest(
  x = predictors,
  y = response,
  training_frame = nirs_data_h2o,
  nfolds = 10,           # Specify 10-fold CV
  fold_assignment = "Modulo",  # Method for fold assignment
  keep_cross_validation_predictions = TRUE,
  seed = 123,
  ntrees = 500
)

# Get overall cross-validation metrics
cv_metrics <- h2o.performance(model, xval = TRUE)
# Print key performance metrics
print(cv_metrics)
# Get detailed metrics if needed
summary(model)
# 6. Clean up
h2o.shutdown()


# Grid search parameters
hyper_params <- list(
  max_depth = c(10, 15, 20, 25, 30),
  ntrees = c(500, 1000, 1500),
  sample_rate = c(0.632, 0.8, 1.0)
)











# 1. Define random grid parameters
hyper_params <- list(
  ntrees = c(500, 750, 1000, 1500),
  max_depth = c(10, 15, 20, 25, 30),
  min_rows = c(1, 3, 5, 10),
  sample_rate = c(0.632, 0.7, 0.8, 0.9),
  mtries = -1  # Will be calculated as sqrt(n_features) by default
)

# 2. Set up random grid search
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors,
  y = response,
  training_frame = nirs_data_h2o,
  nfolds = 10,
  seed = 123,
  hyper_params = hyper_params,
  search_criteria = list(
    strategy = "RandomDiscrete",
    max_models = 20,    # Number of models to try
    max_runtime_secs = 3600,  # Maximum runtime in seconds
    seed = 123
  )
)

# 3. Get results
grid_results <- h2o.getGrid(grid_id = "rf_random_grid", sort_by = "rmse", decreasing = FALSE)
print(grid_results)

# 4. Get best model
best_model <- h2o.getModel(grid_results@model_ids[[1]])