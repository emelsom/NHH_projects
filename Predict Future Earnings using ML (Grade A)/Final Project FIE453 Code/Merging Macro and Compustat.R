# Load necessary libraries
library(tidyverse)

# Read the 'compustat' data
compustat <- read_csv("compustat.csv")


# Calculate the median of NA counts
median_na <- median(colSums(is.na(compustat)))

# Select the columns
compustat <- compustat %>% select_if(function(x) sum(is.na(x)) < median_na)

# Read the 'Macro data'
Macro_data <- read_csv("Macro data.csv")

# Merge the 'compustat' data with 'Macro data' by 'datacqtr'
compustat_with_macro <- merge(compustat, Macro_data, by = "datacqtr")

# Change name from LPERMNO to PERMNO and remove LPERMNO
compustat_with_macro$PERMNO <- compustat_with_macro$LPERMNO
compustat_with_macro <- compustat_with_macro %>% select(-LPERMNO)

# Save the merged data as a CSV file
write_csv(compustat_with_macro, "compustat_with_macro_1.1.csv")










