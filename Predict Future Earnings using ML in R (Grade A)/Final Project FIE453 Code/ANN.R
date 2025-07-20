
# Install Keras if not done before
#install_keras()

# Load necessary packages
library(dplyr)
library(keras)
library(caret)
library(pROC)

# ### ---- Load and Prepare Data ---- ###
# Load the datasets
train <- read.csv("train_lasso_updated.csv")
test <- read.csv("test_lasso_updated.csv")

# Remove identifier columns
excluded_cols <- c("PERMNO", "datacqtr")
train <- train[, !colnames(train) %in% excluded_cols]
test <- test[, !colnames(test) %in% excluded_cols]

# Remove missing values
train <- na.omit(train)
test <- na.omit(test)

# Ensure the target variable is numeric
train$net_income_change <- as.numeric(train$net_income_change)
test$net_income_change <- as.numeric(test$net_income_change)

# Separate features and target variable
train_x <- as.matrix(train[, -which(names(train) == "net_income_change")])
train_y <- as.matrix(train$net_income_change)
test_x <- as.matrix(test[, -which(names(test) == "net_income_change")])
test_y <- as.matrix(test$net_income_change)

# Ensure the number of input units matches the number of features (26 in this case)
input_units <- ncol(train_x)

# Determine the split point 
# Split 35963 includes training data up to 2016Q1
split_point <- 35963

# Create training and validation sets based on time
train_x <- as.matrix(train[1:split_point, -which(names(train) == "net_income_change")])
train_y <- as.matrix(train[1:split_point, "net_income_change"])
validation_x <- as.matrix(train[(split_point+1):nrow(train), -which(names(train) == "net_income_change")])
validation_y <- as.matrix(train[(split_point+1):nrow(train), "net_income_change"])

# ### ---- Build and Compile the Model ---- ###
model <- keras_model_sequential() %>%
  layer_dense(units = input_units, activation = 'relu', input_shape = input_units) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.0003),
  metrics = c('accuracy')
)
set.seed(123)
# ### ---- Train the Model ---- ###
history <- model %>% fit(
  train_x, train_y,
  epochs = 400, 
  batch_size = 32,
  validation_data = list(validation_x, validation_y),
  callbacks = list(
    callback_early_stopping(monitor = 'val_loss', patience = 15)
  )
)


# ### ---- Evaluate the Model on the Test Set ---- ###
test_evaluation <- model %>% evaluate(test_x, test_y)

# Print Test Loss and Test Accuracy
print(paste("Test Loss:", test_evaluation[1]))
print(paste("Test Accuracy:", test_evaluation[2]))

# Make predictions on the test set
test_predictions <- model %>% predict(test_x)

# Convert test predictions to binary outcomes
test_predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

# Evaluate test predictions
test_confusion_matrix <- table(Predicted = test_predicted_classes, Actual = test_y)

# Print test confusion matrix
print(test_confusion_matrix)

# Calculate test accuracy
test_accuracy <- sum(test_predicted_classes == test_y) / length(test_y)
print(paste("Test Set Accuracy:", test_accuracy))

# ### ---- Evaluate the Model on the Training Set ---- ###
train_evaluation <- model %>% evaluate(train_x, train_y)

# Print Train Loss and Train Accuracy
print(paste("Train Loss:", train_evaluation[1]))
print(paste("Train Accuracy:", train_evaluation[2]))

# Make predictions on the training set
train_predictions <- model %>% predict(train_x)

# Convert train predictions to binary outcomes
train_predicted_classes <- ifelse(train_predictions > 0.5, 1, 0)

# Evaluate train predictions
train_confusion_matrix <- table(Predicted = train_predicted_classes, Actual = train_y)

# Print train confusion matrix
print(train_confusion_matrix)

# Calculate train accuracy
train_accuracy <- sum(train_predicted_classes == train_y) / length(train_y)
print(paste("Train Set Accuracy:", train_accuracy))

# ### ---- Confusion Matrix and ROC Curve for Test Set ---- ###
# Confusion Matrix
confusionMatrix(factor(test_predicted_classes), factor(test_y))

# ROC Curve and AUC
roc_curve <- roc(test_y, test_predictions)
plot(
  roc_curve, 
  col = "blue", 
  main = "ROC Curve",
  xlab = "False Positive Rate", 
  ylab = "True Positive Rate"
)
print(paste("AUC:", auc(roc_curve))) 

# ### ---- Plot Training History ---- ###
# Convert history object to data frame for easy plotting
history_df <- data.frame(
  epoch = 1:length(history$metrics$loss),
  loss = history$metrics$loss,
  val_loss = history$metrics$val_loss,
  accuracy = history$metrics$accuracy,
  val_accuracy = history$metrics$val_accuracy
)

# Plot the training and validation loss
plot(history_df$epoch, history_df$loss, type = "l", col = "blue", ylim = range(c(history_df$loss, history_df$val_loss)), xlab = "Epoch", ylab = "Loss", main = "Training and Validation Loss")
lines(history_df$epoch, history_df$val_loss, type = "l", col = "red")
legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)

# Plot the training and validation accuracy
plot(history_df$epoch, history_df$accuracy, type = "l", col = "blue", ylim = range(c(history_df$accuracy, history_df$val_accuracy)), xlab = "Epoch", ylab = "Accuracy", main = "Training and Validation Accuracy")
lines(history_df$epoch, history_df$val_accuracy, type = "l", col = "red")
legend("bottomright", legend = c("Training Accuracy", "Validation Accuracy"), col = c("blue", "red"), lty = 1)
                      
                      
                      
             