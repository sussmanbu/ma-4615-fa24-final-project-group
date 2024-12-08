# Load necessary libraries
library(here)
library(readr)
library(tidyverse)
library(tidyr)

# Load the dataset
dataset_path <- here("admissions_releases_states.csv")
data <- read_csv(dataset_path)

# Display basic information about the dataset before cleaning
str(data)

# Step 1: Convert 'date' column to Date format
data <- data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Step 2: Renaming columns for better readability
data <- data %>%
  rename(
    Date = date,
    Total_Admissions = admissions_total,
    White_Admissions = admissions_white,
    Black_Admissions = admissions_black,
    Hispanic_Admissions = admissions_hispanic,
    AmericanIndian_Admissions = admissions_amerind,
    Asian_Admissions = admissions_asian,
    Other_Admissions = admissions_other,
    Total_Releases = releases_total,
    White_Releases = releases_white,
    Black_Releases = releases_black,
    Hispanic_Releases = releases_hispanic,
    AmericanIndian_Releases = releases_amerind,
    Asian_Releases = releases_asian,
    Other_Releases = releases_other,
    State = state
  )

# Step 3: Checking for duplicate rows
duplicate_rows <- sum(duplicated(data))
print(paste("Number of duplicate rows before cleaning:", duplicate_rows))

# Removing duplicate rows
data <- data[!duplicated(data), ]

# Step 4: Checking for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values per column before cleaning:")
print(missing_values)

# Step 5: Summarizing admissions and releases by race
admissions_totals <- data %>%
  summarise(
    Total_White_Admissions = sum(White_Admissions, na.rm = TRUE),
    Total_Black_Admissions = sum(Black_Admissions, na.rm = TRUE),
    Total_Hispanic_Admissions = sum(Hispanic_Admissions, na.rm = TRUE),
    Total_AmericanIndian_Admissions = sum(AmericanIndian_Admissions, na.rm = TRUE),
    Total_Asian_Admissions = sum(Asian_Admissions, na.rm = TRUE),
    Total_Other_Admissions = sum(Other_Admissions, na.rm = TRUE)
  )

releases_totals <- data %>%
  summarise(
    Total_White_Releases = sum(White_Releases, na.rm = TRUE),
    Total_Black_Releases = sum(Black_Releases, na.rm = TRUE),
    Total_Hispanic_Releases = sum(Hispanic_Releases, na.rm = TRUE),
    Total_AmericanIndian_Releases = sum(AmericanIndian_Releases, na.rm = TRUE),
    Total_Asian_Releases = sum(Asian_Releases, na.rm = TRUE),
    Total_Other_Releases = sum(Other_Releases, na.rm = TRUE)
  )

# Step 6: Converting character columns to factors
data <- data %>%
  mutate_if(is.character, as.factor)

# Step 7: Final check of the cleaned dataset
str(data)

# Step 8: Saving the cleaned dataset
# Saving as an RDS file
cleaned_dataset_path_rds <- here("dataset", "cleaned_admissions_releases_states.rds")
saveRDS(data, file = cleaned_dataset_path_rds)

# Saving as a CSV file (optional)
# cleaned_dataset_path_csv <- here("dataset", "cleaned_admissions_releases_states.csv")
# write_csv(data, cleaned_dataset_path_csv)

