# Load necessary packages
library(tidyverse)
library(caret)
library(glmnet)
library(doParallel)
library(pROC)

# Load the training and test data
train <- read_csv("train_standardized.csv")
test <- read_csv("test_standardized.csv")

# Detect and use multiple cores
cores <- detectCores() - 1
cl <- makeCluster(cores)    
registerDoParallel(cl)      

# Define the Lasso function with time-slicing validation
run_lasso <- function(train_data, target_col, excluded_cols) {
  print("Function run_lasso started")
  
  # Separate numeric predictors and target variable
  numeric_predictors <- train_data %>%
    select(where(is.numeric), -all_of(c(target_col, excluded_cols))) %>%
    as.matrix()  # Convert to matrix for glmnet
  
  print("Numeric predictors selected and converted to matrix")
  
  target <- as.factor(train_data[[target_col]])  # Convert target variable to factor
  levels(target) <- make.names(levels(target))  # Ensure levels are valid R variable names
  print("Target variable extracted and converted to factor with valid levels")
  
  # Check the new levels of target
  print(levels(target))
  
  # Set up time-slice cross-validation
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 35963,  
    horizon = 11929,         
    fixedWindow = TRUE,    
    classProbs = TRUE,     # Use class probabilities for classification
    summaryFunction = twoClassSummary,
    allowParallel = TRUE   # Allow parallel processing
  )
  
  print("TrainControl set up for time-slicing")
  
  # Fit Lasso model using caret::train
  start_time <- Sys.time()  # Start time
  lasso_model <- train(
    x = numeric_predictors,
    y = target,
    method = "glmnet",
    trControl = train_control,
    tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001,0.01, by = 0.001)), #Range found by tuning multiple times
    metric = "ROC",
    family = "binomial"
  )
  end_time <- Sys.time()  # End time
  
  print("Lasso model trained")
  print(paste("Time taken: ", end_time - start_time))
  
  # Extract coefficients at the best lambda
  best_lambda <- lasso_model$bestTune$lambda
  coefficients <- as.data.frame(as.matrix(coef(lasso_model$finalModel, s = best_lambda))) %>%
    rownames_to_column(var = "Variable")
  
  non_zero_vars <- coefficients %>%
    filter(s1 != 0) %>%
    arrange(desc(abs(s1)))
  
  zero_vars <- coefficients %>%
    filter(s1 == 0)
  
  print("Coefficients extracted at best lambda")
  
  return(list(lasso_model = lasso_model, non_zero_vars = non_zero_vars, zero_vars = zero_vars))
}

# Run Lasso on the training dataset
result <- run_lasso(
  train_data = train, 
  target_col = "net_income_change", 
  excluded_cols = "PERMNO"
)

# Check the result
print(result)

# View the most important variables
print(result$non_zero_vars)

# View the variables forced to zero
print(result$zero_vars)

# Exclude zero coefficient variables from the dataset
zero_var_names <- result$zero_vars$Variable

# Ensure we exclude only numeric predictors that were considered in the model
zero_var_names <- zero_var_names[zero_var_names %in% colnames(train)]

# Update the datasets by excluding the zero coefficient variables
train_lasso_updated <- train %>% 
  select(-zero_var_names)

test_lasso_updated <- test %>% 
  select(-zero_var_names)

write_csv(train_lasso_updated, "train_lasso_updated.csv")
write_csv(test_lasso_updated, "test_lasso_updated.csv")


# Preprocess the test data similarly to the training data
numeric_predictors_test <- test %>%
  select(where(is.numeric), -all_of("net_income_change")) %>%
  as.matrix()  # Convert to matrix for glmnet

target_test <- as.factor(test$net_income_change)  # Convert target variable to factor
levels(target_test) <- make.names(levels(target_test))  # Ensure levels are valid R variable names

# Make predictions on the test data
test_predictions <- predict(result$lasso_model, newdata = numeric_predictors_test, type = "prob")[,2]
test_predictions_class <- ifelse(test_predictions > 0.5, "X1", "X0")  # Adjust to match levels

# Convert predictions to factor and set levels to match target_test
test_predictions_class <- factor(test_predictions_class, levels = levels(target_test))

# Evaluate the model
confusion_matrix <- confusionMatrix(test_predictions_class, target_test)

# Calculate AUC
roc_curve <- roc(target_test, as.numeric(test_predictions))
auc_score <- auc(roc_curve)

# Print the evaluation metrics
print(confusion_matrix)
print(paste("AUC Score:", auc_score))