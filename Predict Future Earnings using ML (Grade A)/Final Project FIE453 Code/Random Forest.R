library(ranger)
library(caret)
library(pROC)

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

row_counts_per_date <- train %>%
  group_by(datacqtr) %>%  # Group by the datacqtr column
  summarise(row_count = n()) %>%  # Count the number of observations in each group
  arrange(datacqtr)
print(row_counts_per_date)

# Define time-based validation
time_control <- trainControl(
  method = "timeslice",       # Time-slice validation method
  initialWindow = 35963,      # Training window size
  horizon = 11929,            # Validation window size
  fixedWindow = TRUE,         
  savePredictions = "final",  
  classProbs = TRUE,          # Compute probabilities
  summaryFunction = twoClassSummary, # Use AUC/ROC as metric
  verboseIter = TRUE
)
# Define grid with mtry and ntrees
mtry_values <- c(floor(sqrt(ncol(x_train))), floor(ncol(x_train) / 2), ncol(x_train))
ntrees_values <- c(100, 250, 500, 1000, 1500)  

# Create the tuning grid
grid <- expand.grid(
  mtry = mtry_values, 
  splitrule = "gini",  
  min.node.size = 1
)

# Perform Grid Search Using caret
final_results <- data.frame()  # Initialize results data frame
best_auc <- -Inf               # Initialize best AUC

# Iterate through ntrees values and perform training
for (ntrees in ntrees_values) {
  set.seed(123)
  cat("Evaluating with ntrees =", ntrees, "\n")
  
  model <- train(
    x = x_train,
    y = y_train,
    method = "ranger",          # Use ranger as the method
    trControl = time_control,   # Use time-based validation
    tuneGrid = grid,            
    metric = "ROC",             
    num.trees = ntrees          
  )
  
  # Collect results for this ntrees
  model_results <- model$results
  print(model_results)
  model_results$ntrees <- ntrees
  final_results <- rbind(final_results, model_results)
  
  
}


# Display Results
print(final_results)

# Plot results
ggplot(final_results, aes(x = ntrees, y = ROC, color = as.factor(mtry), group = as.factor(mtry))) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +  
  labs(
    title = "AUC vs Number of Trees for Different mtry Values",
    x = "Number of Trees (ntrees)",
    y = "AUC",
    color = "mtry"  # Legend title
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Find the best parameters
best_row <- final_results[which.max(final_results$ROC), ]
print("Best Parameters:")
print(best_row)

# Extract the best parameters
#best_mtry <- best_row$mtry
best_mtry <- 13 # fixed this to not run the code each time
#best_ntrees <- best_row$ntrees
best_ntrees <- 1500 # fixed this to not run the code each time
cat("Best mtry:", best_mtry, "\n")
cat("Best ntrees:", best_ntrees, "\n")

# Train the final model
final_model <- ranger(
  formula = y_train ~ .,                   
  data = data.frame(y_train, x_train),     
  num.trees = best_ntrees,                 
  mtry = best_mtry,                        
  importance = "impurity",                 
  probability = TRUE,
  verbose = TRUE,
)

# Predict probabilities on the test set
test_predictions <- predict(final_model, data = data.frame(x_test), type = "response")
predicted_probs <- test_predictions$predictions[, 2]  

# Calculate AUC on the test set
numeric_y_test <- as.numeric(y_test)
roc_curve <- roc(numeric_y_test, predicted_probs)
auc_score <- auc(roc_curve)

# Plot the ROC curve
plot(roc_curve,
     col = "blue",         
     lwd = 2,              
     main = "ROC Curve",   
     xlab = "False Positive Rate", 
     ylab = "True Positive Rate"   
)


# Convert probabilities to binary predictions
threshold <- 0.5
predicted_classes <- ifelse(predicted_probs > threshold, "class1", "class0")  # Use threshold of 0.5

# Convert y_test to factor with the same levels as predictions
y_test_factor <- factor(y_test, levels = c("class0", "class1"))

# Calculate Confusion Matrix
conf_matrix <- confusionMatrix(data = factor(predicted_classes, levels = c("class0", "class1")), 
                               reference = y_test_factor)

# metrics
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

print(paste("Test Set AUC:", auc_score))

print(conf_matrix)

# Feature importance
importance_scores <- as.data.frame(final_model$variable.importance)
colnames(importance_scores) <- "Importance"
importance_scores$Feature <- rownames(importance_scores)
rownames(importance_scores) <- NULL

# Sort features by importance
importance_scores <- importance_scores[order(-importance_scores$Importance), ]

# Visualize feature importance
ggplot(importance_scores, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()
