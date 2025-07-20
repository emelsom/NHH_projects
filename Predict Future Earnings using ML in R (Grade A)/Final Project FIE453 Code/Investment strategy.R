predictions <- read_csv("predictions_for_is.csv")
crsp <- read_csv("crsp-monthly.csv")

# Convert `date` to a date format
crsp$date <- as.Date(as.character(crsp$date), format = "%Y%m%d")

# Create `year` and `quarter` columns
crsp <- crsp %>%
  mutate(
    year = year(date),
    quarter = quarter(date)  
  )


# Filter to keep only the last row of each quarter for each company
filtered_data <- crsp %>%
  group_by(PERMNO, year, quarter) %>%
  filter(date == max(date)) %>%
  ungroup()

filtered_data <- filtered_data %>%
  mutate(quarter = paste0("Q", quarter))

#Remove na-values
filtered_data <- na.omit(filtered_data)


# Calculate the quarterly return
filtered_data <- filtered_data %>%
  arrange(PERMNO, date) %>%  # Sort by stock and date
  group_by(PERMNO) %>%
  mutate(
    simple_return = (PRC - lag(PRC)) / lag(PRC)  # Return calculation
  ) %>%
  ungroup()

# Removing variables
filtered_data <- filtered_data %>% dplyr::select(-date)
filtered_data$datacqtr <- paste0(filtered_data$year, filtered_data$quarter)
filtered_data <- filtered_data[, !(names(filtered_data) %in% c("year", "quarter"))]
filtered_data <- filtered_data[, !(names(filtered_data) %in% c("VOL", "SHROUT", "RET", "RETX", "vwretd"))]


# Merging data
data <- merge(predictions, filtered_data[, c("datacqtr", "PERMNO", "PRC", "simple_return")],
                         by = c("datacqtr", "PERMNO"),
                         all.x = TRUE)

data <- na.omit(data)

# Now onto the investment strategy
threshold <- 0.55  # Probability threshold for selecting stocks

# Select stocks based on predictions
selected_stocks <- data %>%
  filter(Predictions > threshold) %>%  
  arrange(datacqtr, desc(Predictions))  


# Calculate portfolio returns 
portfolio_returns <- selected_stocks %>%
  group_by(datacqtr) %>%  # Group by quarter
  summarise(
    portfolio_return = mean(simple_return, na.rm = TRUE)  # Average return for selected stocks
  ) %>%
  ungroup() 

# Calculate cumulative portfolio returns
portfolio_returns <- portfolio_returns %>%
  mutate(
    cumulative_return = cumprod(1 + portfolio_return) - 1  # Compounded return
  )

# Results
print("Portfolio Quarterly Returns:")
print(portfolio_returns)

