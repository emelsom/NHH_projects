library(dplyr)
library(lubridate)

data <- read.csv("wharton_backtest.csv")

# Convert `fdate` to a date format
data$fdate <- as.Date(data$fdate)

# Create `year` and `quarter` columns
data <- data %>%
  mutate(
    year = year(fdate),
    quarter = quarter(fdate)  
  )

  

# Filter to keep only the last row of each quarter for each company
filtered_data <- data %>%
  group_by(PERMNO, year, quarter) %>%
  filter(fdate == max(fdate)) %>%
  ungroup()

filtered_data <- filtered_data %>%
  mutate(quarter = paste0("Q", quarter))

filtered_data <- filtered_data %>% dplyr::select(-fdate, -date, -signal)

# Making a function that adds a lagged variable
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

filtered_data <- filtered_data %>% 
  group_by(PERMNO) %>% 
  add_lag(columns_indices = 2)
}# View the resulting dataset
print(filtered_data)


# Save the filtered data to a new CSV
write.csv(filtered_data, "filtered_wharton_backtest.csv", row.names = FALSE)

