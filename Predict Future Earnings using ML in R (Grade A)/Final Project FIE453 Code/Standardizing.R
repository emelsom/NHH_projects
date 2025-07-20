library(tidyverse)
library(caret)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Identify numeric columns to standardize
standardizing_columns <- train %>%
  dplyr::select(-c(1:187)) %>%  # Exclude macro data
  select_if(~ length(unique(.)) > 2) %>%  # Exclude binary variables
  names()

library(doParallel)
library(caret)

# Activate parallell computing
cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(cores)   # Create a cluster
registerDoParallel(cl)     # Register the cluster for parallel processing

# Create preProcess object using training data
preproc <- preProcess(train[, standardizing_columns], method = c("center", "scale"))


# Apply standardization to training set
system.time({
  train_standardized <- train %>%
    mutate(across(all_of(standardizing_columns), ~ {
      predict(preproc, newdata = train[, standardizing_columns])[[cur_column()]]
    }))
})


# Apply standardization to test set
test_standardized <- test %>%
  mutate(across(all_of(standardizing_columns), ~ {
    predict(preproc, newdata = test[, standardizing_columns])[[cur_column()]]
  }))

# Convert net_income_change to a factor
train_standardized$net_income_change <- as.factor(train_standardized$net_income_change)
test_standardized$net_income_change <- as.factor(test_standardized$net_income_change)

# Save the train/validation and test set as separate csv files
write.csv(train_standardized, "train_standardized.csv", row.names = FALSE)
write.csv(test_standardized, "test_standardized.csv", row.names = FALSE)

# Stop the cluster 
stopCluster(cl)
registerDoSEQ()  # Reset



