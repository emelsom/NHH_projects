# Load required libraries
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(doParallel)

# Load data
train <- read.csv("train_lasso_updated.csv")
test <- read.csv("test_lasso_updated.csv")

excluded_cols <- c("PERMNO", "datacqtr")

train <- train[, !colnames(train) %in% excluded_cols ]
test <- test[, !colnames(test) %in% excluded_cols]

# Remove rows with missing values
train <- na.omit(train)
test <- na.omit(test)


# Convert net_income_change to factor
train$net_income_change <- factor(train$net_income_change, 
                                  levels = c(0, 1), 
                                  labels = c("Negative_Change", "Positive_Change"))
test$net_income_change <- factor(test$net_income_change, 
                                 levels = c(0, 1), 
                                 labels = c("Negative_Change", "Positive_Change"))


# Time series cross-validation control
time_control <- trainControl(
  method = "timeslice",
  initialWindow = 35963,  # Initial training window
  horizon = 11929,        # Prediction horizon
  fixedWindow = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE  # This will print progress during training
)

# Define the formula with all variables
formula <- net_income_change ~ .

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)  # Leave one core for the system
registerDoParallel(cl)

# Define hyperparameter tuning grid
tune_grid <- expand.grid(cp = 0.000450001)
#tune_grid <- expand.grid(cp = seq(0.000001, 0.001, by = 0.00001))  

# Train the classification tree model
set.seed(123)
time_series_model <- train(
  formula,
  data = train,
  method = "rpart",
  trControl = time_control,
  tuneGrid = tune_grid,  # Explicit tuning grid
  metric = "ROC"  
)
# Assuming the time_series_model has been trained as per your provided code

# Extract the complexity table
cp_table <- time_series_model$finalModel$cptable


# Extract data from cp_table
nsplit <- cp_table[, "nsplit"]
rel_error <- cp_table[, "rel error"]
cp <- cp_table[, "CP"]

# Set up plot margins for dual x-axes
par(mar = c(5, 4, 4, 4) + 1.5)

# Plot relative error against number of splits
plot(nsplit, rel_error, 
     type="o", 
     xlab="Number of Splits (Tree Size)", 
     ylab="Relative Error",
     col="blue")

# Add top x-axis for CP
axis(3, at = nsplit, labels = round(cp, 6))  # Top axis with CP values
mtext("Complexity Parameter (CP)", side = 3, line = 2.1)

# Add the main title and move it higher
title(main = "Model Complexity Analysis", line = 4)  # Increase the line number to move higher

# Highlight minimum error point
min_error_index <- which.min(rel_error)
points(nsplit[min_error_index], rel_error[min_error_index], 
       col="red", 
       pch=16, 
       cex=1.5)

# Add a legend
legend("topright", 
       legend = c("Relative Error", "Optimal Point"),
       col = c("blue", "red"), 
       pch = c(1, 16), 
       lty = 1)

# Find and print details
optimal_cp <- cp[min_error_index]
optimal_splits <- nsplit[min_error_index]
print(paste("Optimal CP:", optimal_cp))
print(paste("Number of Splits at Optimal CP:", optimal_splits))

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Predict on test data
test_predictions <- predict(time_series_model, newdata = test, type = "prob")

# Confusion matrix
predicted_classes <- ifelse(test_predictions[, "Positive_Change"] > 0.5, "Positive_Change", "Negative_Change")
conf_matrix <- confusionMatrix(as.factor(predicted_classes), test$net_income_change)

# Print results
print("Model Performance:")
print(conf_matrix)

# Extract and print accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy: ", accuracy))

# Plot the decision tree
rpart.plot(
  time_series_model$finalModel, 
  type = 3, 
  extra = 102, 
  fallen.leaves = TRUE, 
  main = "Time Series Classification Tree"
)

# ROC Curve
roc_curve <- roc(test$net_income_change, test_predictions[, "Positive_Change"])
# Plot the ROC Curve with updated axis labels
plot(
  roc_curve, 
  col = "blue", 
  main = "ROC Curve",
  xlab = "False Positive Rate", 
  ylab = "True Positive Rate"
)

auc <- auc(roc_curve)
print(paste("AUC: ", auc))

# Variable importance
var_importance <- varImp(time_series_model, scale = FALSE)

# Print variable importance
print("Variable Importance:")
print(var_importance)

# Plot variable importance
plot(var_importance, top = ncol(train) -1, main = "Top Variable Importance")






