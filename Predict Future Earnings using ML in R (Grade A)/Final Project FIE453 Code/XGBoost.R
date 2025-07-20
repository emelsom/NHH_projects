library(xgboost)
library(caret)
library(pROC)
library(dplyr)
library(ggplot2)

# Load Data
train <- read.csv("train_lasso_updated.csv")
test <- read.csv("test_lasso_updated.csv")

# Prepare Data
x_train <- train[, 4:ncol(train)]  
y_train <- as.factor(train$net_income_change) 

x_test <- test[, 4:ncol(test)]
y_test <- as.factor(test$net_income_change)

levels(y_train) <- c("class0", "class1")
levels(y_test) <- c("class0", "class1")

# Define time-based validation
time_control <- trainControl(
  method = "timeslice",
  initialWindow = 35963,   
  horizon = 11929,         # Validation window size 
  fixedWindow = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary, # We will use AUC (ROC) as main metric
  verboseIter = TRUE
)

# tuning grid for xgboost
tune_grid <- expand.grid(
  nrounds = c(100, 250, 500, 750, 1000),         # ntrees
  max_depth = c(3, 6, 9),             # tree depth
  eta = c(0.01, 0.1, 0.3),            # shrinkage parameter
  gamma = 0,                          # minimum loss reduction
  colsample_bytree = 1,               # subsample of columns
  min_child_weight = 1,               # minimum sum of instance weight
  subsample = c(0.8,1)                # subsample of rows
)

# Train the xgboost model with caret
set.seed(123)
xgb_model <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  trControl = time_control,
  tuneGrid = tune_grid,
  metric = "ROC",
)


# Display results
print(xgb_model)
final_results <- xgb_model$results
print(final_results)

# Find the best parameters
best_params <- xgb_model$bestTune
print("Best Parameters:")
print(best_params)

# Plot AUC vs Number of Trees, colored by eta or max_depth
# visualize by eta and facet by max_depth
ggplot(final_results, aes(x = nrounds, y = ROC, color = factor(eta), group = interaction(eta, max_depth))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ max_depth, scales = "free_y") +
  labs(
    title = "AUC vs nrounds for Different eta and max_depth Values",
    x = "Number of Rounds (Trees)",
    y = "AUC",
    color = "eta"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Retrain the final model on the entire training set with best parameters
final_xgb <- xgboost(
  data = as.matrix(x_train),
  label = as.numeric(y_train) - 1,  # xgboost expects 0/1, was 1/2
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  objective = "binary:logistic", # To output probability, for classification
  eval_metric = "auc",
  verbose = 0
)

# Predict on the test set
test_preds <- predict(final_xgb, as.matrix(x_test))
roc_curve <- roc(as.numeric(y_test), test_preds)
auc_score <- auc(roc_curve)

# Save the predictions for investment strategy
test_preds_df <- data.frame(Predictions = test_preds)
x_test_subset <- test[, 1:3]  
combined_df <- cbind(x_test_subset, test_preds_df)
write.csv(combined_df, "predictions_for_is.csv", row.names = FALSE)

# Plot the roc curve
plot(roc_curve,
     col = "blue",         
     lwd = 2,              
     main = "ROC Curve",   
     xlab = "False Positive Rate", 
     ylab = "True Positive Rate"   
)

# Binary classification 
predicted_classes <- ifelse(test_preds > 0.5, "class1", "class0")

# Confusion matrix
y_test_factor <- factor(y_test, levels = c("class0", "class1"))
conf_matrix <- confusionMatrix(data = factor(predicted_classes, levels = c("class0", "class1")), 
                               reference = y_test_factor)

# Extract metrics
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
cat("Test Set AUC:", auc_score, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

print(conf_matrix)

# Feature importance
importance <- xgb.importance(model = final_xgb)

# Convert importance to a data frame
importance_scores <- as.data.frame(importance)

# Visualize feature importance
ggplot(importance_scores, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()
