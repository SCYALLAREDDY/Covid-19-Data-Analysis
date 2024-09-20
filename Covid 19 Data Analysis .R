# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
covid_data <- read_csv("/Users/yallareddysadumchinnapareddigari/Desktop/covid_data.csv")

# Inspect the data
glimpse(covid_data)

# Filter and select relevant data
filtered_data <- covid_data %>%
  select(country = location, total_tests = total_tests, positive_cases = total_cases) %>%
  filter(!is.na(total_tests) & !is.na(positive_cases) & total_tests > 0)

# Aggregate data by country
aggregated_data <- filtered_data %>%
  group_by(country) %>%
  summarize(
    total_tests = sum(total_tests, na.rm = TRUE),
    total_positive_cases = sum(positive_cases, na.rm = TRUE),
    positive_rate = (total_positive_cases / total_tests) * 100,
    .groups = 'drop'
  )

# Identify top countries
top_countries <- aggregated_data %>%
  arrange(desc(positive_rate)) %>%
  head(10)

# Visualize the data
ggplot(top_countries, aes(x = reorder(country, -positive_rate), y = positive_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Countries by COVID-19 Positive Rate",
       x = "Country",
       y = "Positive Rate (%)") +
  theme_minimal() +
  coord_flip()

top_country_names <- top_countries$country
top_positive_rates <- top_countries$positive_rate

results_list <- list(
  top_countries = top_country_names,
  positive_rates = top_positive_rates,
  total_tests = top_countries$total_tests,
  total_positive_cases = top_countries$total_positive_cases
)

correlation <- cor(aggregated_data$total_tests, aggregated_data$total_positive_cases)
print(paste("Correlation between total tests and positive cases: ", correlation))

# Assuming the correct names for country and date are 'location' and 'date'
covid_time_series <- covid_data %>%
  group_by(location, date) %>%  # Use actual column names here
  summarize(
    total_tests = sum(total_tests, na.rm = TRUE),  # Adjust if necessary
    total_positive_cases = sum(total_cases, na.rm = TRUE),  # Adjust if necessary
    .groups = 'drop'
  )

covid_time_series <- covid_time_series %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(
    positivity_rate = (total_positive_cases / total_tests) * 100,
    daily_new_cases = total_positive_cases - lag(total_positive_cases, default = 0),
    daily_tests = total_tests - lag(total_tests, default = 0)
  ) %>%
  ungroup()

selected_country <- "United States"  # Change as needed
ggplot(covid_time_series %>% filter(location == selected_country), aes(x = date)) +
  geom_line(aes(y = positivity_rate), color = "blue", size = 1) +
  labs(title = paste("COVID-19 Positivity Rate Over Time in", selected_country),
       x = "Date",
       y = "Positivity Rate (%)") +
  theme_minimal()

countries_to_compare <- c("United States", "India", "Brazil")  # Change as needed
ggplot(covid_time_series %>% filter(location %in% countries_to_compare), 
       aes(x = date, color = location)) +
  geom_line(aes(y = positivity_rate), size = 1) +
  labs(title = "COVID-19 Positivity Rates Over Time",
       x = "Date",
       y = "Positivity Rate (%)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

