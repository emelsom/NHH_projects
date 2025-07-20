library(readr)
library(dplyr)
compustat <- read_csv("compustat_with_macro_1.1.csv")

# Creating the net income increase or decrease variable, and removing 
# na-values from net income
compustat <- compustat[!is.na(compustat$niq), ]

compustat <- compustat %>%
  group_by(PERMNO) %>%  # Group by company
  arrange(datacqtr, .by_group = TRUE) %>%  # Sort within each company
  mutate(
    net_income_change = ifelse(lag(niq) < niq, 1, 0)  # Compare with the previous quarter
  ) %>%
  ungroup()  # Remove grouping after calculation

# Get the names of the macro columns to lag
columns_to_lag <- names(compustat)[187:370]

# Create lagged macro variables
for (col in columns_to_lag) {
  lagged_col_name <- paste0(col, "_lag")  # New column name
  compustat <- compustat %>%
    mutate(!!lagged_col_name := dplyr::lag(.data[[col]], n = 1))
}

# Count the number of observations in each category
category_counts <- table(compustat$net_income_change)
print(category_counts)
  

# Removing na-values
# Calculate the proportion of NA values for each column
na_proportions <- colSums(is.na(compustat)) / nrow(compustat)
# Filter columns with more than 5% NA values
columns_with_na <- na_proportions[na_proportions > 0.05]

# Set threshold
threshold <- 0.05

# Identify columns with more missing values than the threshold
columns_to_remove <- names(na_proportions[na_proportions > threshold])

# Remove those columns from the data frame
compustat <- compustat[, !(names(compustat) %in% columns_to_remove)]


# Remove other irrelevant columns
compustat <- compustat %>% 
  dplyr::select(-GVKEY:-datafqtr, -costat)


# Here we add lagged variables for all the company specific data
add_lag <- function(data, columns_indices) {
  
  # Get the names of the columns to lag
  columns_to_lag <- names(data)[columns_indices]
  
  # Create lagged variables
  for (col in columns_to_lag) {
    lagged_col_name <- paste0(col, "_lag")  # New column name
    data <- data %>%
      group_by(PERMNO) %>%
      mutate(!!lagged_col_name := dplyr::lag(.data[[col]], n = 1)) %>%
      ungroup()
  }
  
  return(data)
}

# Use the add_lag function to add lagged variables to the data set
compustat_clean <- compustat %>% 
  group_by(PERMNO) %>% 
  add_lag(columns_indices = 2:80)

# Removing the first row of each company as the lagged variables here is all NA's
compustat_clean <- compustat_clean %>%
  group_by(PERMNO) %>% 
  slice(-1) %>% 
  ungroup()

# Removes all companies with any NA's so we are left with a complete panel
compustat_clean <- compustat_clean %>%
  group_by(PERMNO) %>% 
  mutate(
    has_na = if_any(everything(), is.na)
  ) %>% 
  ungroup() %>% 
  filter(!PERMNO %in% PERMNO[has_na]) %>% 
  dplyr::select(-has_na)



# Adding beta
wharton <- read_csv("filtered_wharton_backtest.csv")

# Making datacqtr in wharton
wharton$datacqtr <- paste0(wharton$year, wharton$quarter)
wharton <- wharton[, !(names(wharton) %in% c("quarter"))]
# Merge the data
compustat_clean <- merge(compustat_clean, wharton[, c("datacqtr", "PERMNO", "year", "beta_lag")],
                   by = c("datacqtr", "PERMNO"),
                   all.x = TRUE)


# Group by year and calculate the proportion of missing beta values
beta_missing_by_year <- compustat_clean %>%
  group_by(year) %>%
  summarize(
    total_rows = n(),
    missing_beta = sum(is.na(beta_lag)),
    proportion_missing = missing_beta / total_rows
  )

print(beta_missing_by_year)
compustat_clean <- compustat_clean %>% 
  dplyr::select(-year)

'''
There is no signinficant patterns regarding the missing values of betas the
different years. 
'''
# Group by PERMNO and calculate the proportion of missing beta values
beta_missing_by_permno <- compustat_clean %>%
  group_by(PERMNO) %>%
  summarize(
    total_rows = n(),
    missing_beta = sum(is.na(beta_lag)),
    proportion_missing = missing_beta / total_rows
  ) %>%
  arrange(desc(proportion_missing)) # Arrange in descending order 

# Filter to show companies with a high proportion of missing beta values (e.g., > 50%)
high_missing_companies <- beta_missing_by_permno %>%
  filter(proportion_missing > 0.5)

print(high_missing_companies)

'''
After some analysis of Beta, there seems to be a few companies missing a lot of 
beta values. Therefore these companies are removed from the dataset. 
'''
# Remove companies with a high proportion of missing beta values
compustat_clean <- compustat_clean %>%
  filter(!(PERMNO %in% high_missing_companies$PERMNO))


# Find the first column name that contains "_lag" and subtracts 1 to include
#the target variable net_income_change
lag_col_index <- (which(str_detect(names(compustat_clean), "_lag"))[1])-1

# Select columns from that index onward (removing irrelevant columns for ML)
compustat_clean <- compustat_clean %>%
  dplyr::select(datacqtr, PERMNO, lag_col_index:ncol(compustat_clean))


# Now we split into train/test/validation split
train <- compustat_clean %>% filter(datacqtr < "2018Q2")
#train <- compustat_clean %>% filter(datacqtr < "2016Q2")
#validation <- compustat_clean %>% filter(datacqtr >= "2016Q2" & datacqtr < "2018Q2")
test <- compustat_clean %>% filter(datacqtr >= "2018Q2")


# Here we impute the missing values for beta 
# Calculate mean beta by company in the training set
train_means <- train %>%
  group_by(PERMNO) %>%
  summarize(mean_beta = mean(beta_lag, na.rm = TRUE))

# Impute beta in each dataset
train <- train %>%
  left_join(train_means, by = "PERMNO") %>%
  mutate(beta_lag = ifelse(is.na(beta_lag), mean_beta, beta_lag)) %>%
  dplyr::select(-mean_beta)

test <- test %>%
  left_join(train_means, by = "PERMNO") %>%
  mutate(beta_lag = ifelse(is.na(beta_lag), mean_beta, beta_lag)) %>%
  dplyr::select(-mean_beta)


# Removes all companies with any NA's in beta_lag so we are left with a complete panel
remove_beta_na <- function(df){
  df <- df %>%
    group_by(PERMNO) %>% 
    mutate(
      has_na = if_any(beta_lag, is.na)
    ) %>% 
    ungroup() %>% 
    filter(!PERMNO %in% PERMNO[has_na]) %>% 
    dplyr::select(-has_na)
  return(df)
}

#Applies the function above to the splits
train <- remove_beta_na(train)
#validation <- remove_beta_na(validation)
test <- remove_beta_na(test)

#Exports the final data set splits as csv's
write_csv(train, "train.csv")
write_csv(test, "test.csv")

