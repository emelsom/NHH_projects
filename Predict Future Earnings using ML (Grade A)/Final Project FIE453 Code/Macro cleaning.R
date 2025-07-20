library(tidyverse)
library(here)

BLS_data <- read_csv(here("Macro data", "BLS.csv"))

BLS_data <- BLS_data %>% 
  filter(Period %in% c("M03", "M06", "M09", "M12")) %>% 
  mutate(Value = 
           100*(Value - lag(Value, default = 1))/lag(Value, default = 1),
         Quarter = case_when(
           Period == "M03" ~ "Q1",
           Period == "M06" ~ "Q2",
           Period == "M09" ~ "Q3",
           Period == "M12" ~ "Q4",
           .default = Period
         ),
         Metric = "Employment"
         ) %>% 
  filter(Year >= 2010) %>% 
  select(Metric, Year, Quarter, Value) %>% 
  pivot_wider(
    names_from = Metric,
    values_from = Value
  )

Merge_BEA <- function(files){
  
  # Reads the files, adjusts column names to match index, and puts them into a table
  tables <- lapply(seq_along(files), function(i){
    table <- read_csv(here("Macro data/", files[i]))
    colnames(table) <- seq_len(ncol(table))
    return(table)
  })
  
  # Removes the first two rows of the first table in tables as these were just NA's
  tables[[1]] <- tables[[1]][-(1:2),]
  
  # Loop that retrieves each table, filters out non-data rows, and binds it to
  # table_1
  for(i in 2:length(tables)){
    table <- tables[[i]]
    table <- table[!is.na(as.numeric(table[[1]])), ]
    tables[[1]] <- rbind(tables[[1]], table)
  }
  # Fixes the column names to be a combination of year and quarter
  # instead of an arbitrary number 
  colnames(tables[[1]]) <- apply(tables[[1]][(1:2),], 2, paste, collapse = "_")
  
  #Adjusts the column name for the second column to be "Metric"
  colnames(tables[[1]])[2] <- "Metric"
  
  # Returns the table minus the first column that only included indices
  return(tables[[1]][,-1])
}

test <- Merge_BEA(filenames)


BEA_data <- function(files){
  data <- Merge_BEA(files)
  data <- data[-c(1, 2), ] %>% 
    group_by(Metric) %>%
    mutate(Metric = paste(Metric, seq_along(Metric), sep = "_")) %>%
    ungroup() %>% 
    pivot_longer(
      !"Metric",
      names_to = "Period",
      values_to = "Value"
    ) %>% 
    separate_wider_delim(
      cols = Period,
      names = c("Year","Quarter"),
      delim = "_"
    )
  data$Value <- as.numeric(data$Value)
  data$Year <- as.numeric(data$Year)
  data <- data %>% 
    filter(!is.na(Value))
  return(data)
}

# Vector containing all file names for the BEA tables used 
filenames <- c("BEA table 111.csv", "BEA table 231.csv", "BEA table 391.csv", "BEA table 421.csv", "BEA table 531.csv")

# Stores a data frame with all data from BEA formatted by calling on the BEA_data function
Macro_data <- BEA_data(filenames)

# Adds the Employment data from BLS to the data frame
Macro_data <- Macro_data %>% 
  add_row(
    Metric = "Employment_Change",
    Year = BLS_data$Year,
    Quarter = BLS_data$Quarter,
    Value = BLS_data$Employment
  )

# Reads and formats risk free rates to fit the data frame
rf_rate <- read_csv(here("Macro data", "DGS10.csv"))
rf_rate <- rf_rate %>% 
  mutate(
    Year = year(DATE),
    Quarter = quarter(DATE),
    Quarter = case_when(
      Quarter == 1 ~ "Q1",
      Quarter == 2 ~ "Q2",
      Quarter == 3 ~ "Q3",
      Quarter == 4 ~ "Q4"
    ),
    Metric = "DGS10"
  ) %>% 
  select(-DATE)

# Adds risk free rates to the data frame
Macro_data <- Macro_data %>% 
  add_row(
    Metric = rf_rate$Metric,
    Year = rf_rate$Year,
    Quarter = rf_rate$Quarter,
    Value = rf_rate$DGS10
  )

# Creates a column with year and quarter matching the syntax in compustat
Macro_data$datacqtr <- paste(Macro_data$Year, Macro_data$Quarter, sep = "")

# Converts the data frame into a wide format ready for export
Macro_data <- Macro_data %>% 
  select(-Year, -Quarter) %>% 
  pivot_wider(
    names_from = Metric,
    values_from = Value
  )

# Saves the data frame as a CSV file
write_csv(Macro_data, "Macro data.csv")
