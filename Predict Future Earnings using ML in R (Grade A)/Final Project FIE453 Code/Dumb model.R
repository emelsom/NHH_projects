# Load libraries
library(caret)

# Load data
train <- read.csv("train_lasso_updated.csv")
test <- read.csv("test_lasso_updated.csv")

excluded_cols <- c("PERMNO", "datacqtr")

train <- train[, !colnames(train) %in% excluded_cols ]
test <- test[, !colnames(test) %in% excluded_cols]

# Always predict 1 (increase)
test_predictions <- rep(1, nrow(test))

# Convert the net_income_change column to a factor, as required by confusionMatrix
test$net_income_change <- as.factor(test$net_income_change)

# Always predict 1 (increase)
test_predictions <- factor(rep(1, nrow(test)), levels = levels(test$net_income_change))

# Check model performance
confusion_matrix <- confusionMatrix(
  data = test_predictions,        # Predicted values
  reference = test$net_income_change  # Actual values
)

# Print confusion matrix
print(confusion_matrix)


# Show class distribution
print("Training set distribution:")
print(table(train$net_income_change))


# Load necessary library for AUC calculation
library(pROC)

# Convert net_income_change to numeric for AUC calculation (1 = increase, 0 = decrease)
test$net_income_change_numeric <- as.numeric(test$net_income_change) - 1  # Convert to 0/1

# Always predict probability of 1 for increase
test_predictions_prob <- rep(1, nrow(test))  # Probabilities for predicting '1'

# Calculate AUC
roc_curve <- roc(test$net_income_change_numeric, test_predictions_prob)
auc_value <- auc(roc_curve)

# Print AUC
print(paste("AUC:", auc_value))

# Optional: Plot ROC Curve
plot(roc_curve, main = "ROC Curve for Always Predicting '1'")
