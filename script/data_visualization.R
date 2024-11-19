# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)  # For handling dates


# Import dataset
vaccinations <- read.csv("data/us_state_vaccinations.csv", stringsAsFactors = FALSE)

# Convert 'date' column to Date format
vaccinations$date <- as.Date(vaccinations$date, format = "%Y-%m-%d")

#Filter data
filtered_data <- vaccinations %>%
  filter(location %in% c("Alaska", "District of Columbia") & year(date) %in% c(2021, 2023))

# Check for missing values
sum(is.na(filtered_data$daily_vaccinations))

# Remove rows with missing daily vaccinations
filtered_data <- filtered_data %>%
  filter(!is.na(daily_vaccinations))

# Summarize mean daily vaccinations by location and year
summary_table <- filtered_data %>%
  group_by(location, year = year(date)) %>%
  summarize(mean_daily_vaccinations = mean(daily_vaccinations, na.rm = TRUE))

# View the summary table
print(summary_table)

# Bar plot for mean daily vaccinations
ggplot(summary_table, aes(x = as.factor(year), y = mean_daily_vaccinations, fill = location)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Mean Daily COVID Vaccinations in Alaska and DC (2021 vs. 2023)",
    x = "Year",
    y = "Mean Daily Vaccinations"
  ) +
  theme_minimal()