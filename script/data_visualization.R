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


# Plot histogram for daily vaccinations
ggplot(filtered_data, aes(x = daily_vaccinations)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~location + year(date)) +
  labs(
    title = "Histogram with Density Plot of Daily Vaccinations",
    x = "Daily Vaccinations",
    y = "Density"
  ) +
  theme_minimal()


# Split data by location and year
data_2021 <- filtered_data %>% filter(year(date) == 2021)
data_2023 <- filtered_data %>% filter(year(date) == 2023)

# Shapiro-Wilk test for 2021
shapiro_alaska_2021 <- shapiro.test(data_2021 %>% filter(location == "Alaska") %>% pull(daily_vaccinations))
shapiro_dc_2021 <- shapiro.test(data_2021 %>% filter(location == "District of Columbia") %>% pull(daily_vaccinations))

# Shapiro-Wilk test for 2023
shapiro_alaska_2023 <- shapiro.test(data_2023 %>% filter(location == "Alaska") %>% pull(daily_vaccinations))
shapiro_dc_2023 <- shapiro.test(data_2023 %>% filter(location == "District of Columbia") %>% pull(daily_vaccinations))

# Print results
shapiro_alaska_2021
shapiro_dc_2021
shapiro_alaska_2023
shapiro_dc_2023

# Add log-transformed column
filtered_data <- filtered_data %>%
  mutate(log_daily_vaccinations = log1p(daily_vaccinations))

# Histogram for log-transformed data
ggplot(filtered_data, aes(x = log_daily_vaccinations)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~location + year(date)) +
  labs(
    title = "Log-Transformed Histogram with Density Plot",
    x = "Log(Daily Vaccinations + 1)",
    y = "Density"
  ) +
  theme_minimal()

# Perform Wilcoxon test (non-parametric alternative)
wilcox_test <- wilcox.test(log_daily_vaccinations ~ location, data = filtered_data)
wilcox_test
