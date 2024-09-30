#Assignment 4 
#By Jose Fuentes

#1rst and 2nd Part
# Load the necessary library that contains tidyr and dplyr
library(tidyverse)

# Read the CSV file using path
data <- read.csv("C:/Users/Dell/Downloads/assignment4.csv")

# Check the initial structure and column names
print("Initial Column Names:")
print(colnames(data))

print("Structure of the Data:")
str(data)

# Display the first few rows of the original dataset
print("First Few Rows of the Original Data:")
print(head(data))

# Check for any NA values present in the dataset
print("Summary of NA Values:")
print(summary(data))

# Check the dimensions of the data
print("Dimensions of the Data:")
print(dim(data))

# Clean and tidy the dataset:
data <- data %>%
  # Remove rows that are all NA
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  # Select relevant columns with the correct original names
  select(Airline = X, Status = X.1, 
         Los_Angeles = `Los.Angeles`, 
         Phoenix, 
         San_Diego = `San.Diego`, 
         San_Francisco = `San.Francisco`, 
         Seattle) %>%
  # Convert the relevant city columns to numeric values:
  mutate(across(Los_Angeles:Seattle, as.numeric)) %>%
  # Filter for only on-time flights
  filter(Status == "on time") %>%
  # Reshape data into a longer format
  pivot_longer(cols = Los_Angeles:Seattle, names_to = "City", values_to = "Count")

# Display the cleaned data
print("Cleaned Data:")
print(head(data))

#Part3
# Loading the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Reading the CSV file
data <- read.csv("C:/Users/Dell/Downloads/assignment4.csv")

# Clean and tidy the dataset:
data <- data %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%  # Remove rows with all NA values
  select(-X) %>%  # Remove the unnecessary 'X' column
  rename(
    Airline = `X.1`,  
    Los_Angeles = `Los.Angeles`,
    Phoenix = `Phoenix`,
    San_Diego = `San.Diego`,
    San_Francisco = `San.Francisco`,
    Seattle = `Seattle`
  ) %>%
  mutate(across(Los_Angeles:Seattle, as.numeric)) %>%
  filter(!is.na(Airline), Airline != "")  # Remove rows with NA or empty Airlines

# Verify the cleaned data and column names
print("Cleaned Data:")
print(head(data))
print("Column Names After Cleaning:")
print(colnames(data))

# Filter for delayed flights:
delayed_flights <- data %>%
  filter(grepl("delayed", Airline))  # Ensure we capture 'delayed'

# Debugging: Check delayed flights
print("Delayed Flights Data:")
print(head(delayed_flights))
print(paste("Number of delayed flights:", nrow(delayed_flights)))

# Check the column names again for delayed flights
print("Column Names in Delayed Flights Data:")
print(colnames(delayed_flights))

# Summarize the arrival delays for each airline
delay_summary <- delayed_flights %>%
  group_by(Airline) %>%
  summarize(Total_Delays = sum(c_across(Los_Angeles:Seattle), na.rm = TRUE), .groups = 'drop')

# Debugging: Check the delay summary
print("Arrival Delays Summary:")
print(delay_summary)

# Now Create a bar plot to compare delays if there are valid delays in dataset:
if(nrow(delay_summary) > 0 && all(!is.na(delay_summary$Total_Delays) & delay_summary$Total_Delays > 0)) {
  ggplot(delay_summary, aes(x = Airline, y = Total_Delays, fill = Airline)) +
    geom_bar(stat = "identity") +
    labs(title = "Comparison of Arrival Delays for Airlines",
         x = "Airline",
         y = "Total Delays") +
    theme_minimal()
} else {
  print("No valid delays to display in the plot.")
}

