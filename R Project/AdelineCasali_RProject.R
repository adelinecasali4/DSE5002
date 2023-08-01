# R Project
# Adeline Casali


# (1) Load packages and data ----------------------------------------------

# Load packages
library(tidyverse)

# Load data
salaries_df <- read_csv("R Project/data.csv")


# (2) Data cleaning and exploration ------------------------------------------

head(salaries_df)

summary(salaries_df)

# Filter for only companies from the US
salaries_df_us <- filter(salaries_df, company_location == "US")

summary(salaries_df_us)

# (3) Salary trends over time ----------------------------------------------

# Create a df containing median salary and IQR for each year
salaries_med_iqr <- salaries_df %>% 
  group_by(work_year) %>% 
  summarize(median_salaries_usd = median(salary_in_usd), 
            q1 = quantile(salary_in_usd, 0.25), 
            q3 = quantile(salary_in_usd, 0.75))

print(salaries_med_iqr)

# Plot the results
ggplot(salaries_med_iqr, aes(x = work_year)) + 
  geom_line(aes(y = median_salaries_usd), color = "blue") + 
  geom_ribbon(aes(ymin = q1, ymax = q3), 
              fill = "lightblue", alpha = 0.5) + 
  geom_point(aes(y = median_salaries_usd), color = "blue", size = 1) + 
  scale_y_continuous(labels = scales::comma_format(), limits = c(40000, 200000)) + 
  scale_x_continuous(breaks = as.numeric(unique(salaries_med_iqr$work_year)), 
                     labels = c("2020", "2021", "2022")) + 
  labs(title = "Median Data Science Salary (USD) over 2020 - 2022, all companies", 
       subtitle = "Shaded area represents interquartile range (Q1 to Q3)", 
       x = "Year", y = "Median Salary (USD)")

# Create a df containing median salary and IQR for each year, US-based only
salaries_med_iqr_us <- salaries_df_us %>% 
  group_by(work_year) %>% 
  summarize(median_salaries_usd = median(salary_in_usd), 
            q1 = quantile(salary_in_usd, 0.25), 
            q3 = quantile(salary_in_usd, 0.75))

print(salaries_med_iqr_us)

# Create the same plot but with only US-based companies
ggplot(salaries_med_iqr_us, aes(x = work_year)) + 
  geom_line(aes(y = median_salaries_usd), color = "blue") + 
  geom_ribbon(aes(ymin = q1, ymax = q3), 
              fill = "lightblue", alpha = 0.5) + 
  geom_point(aes(y = median_salaries_usd), color = "blue", size = 1) + 
  scale_y_continuous(labels = scales::comma_format(), limits = c(40000, 200000)) + 
  scale_x_continuous(breaks = as.numeric(unique(salaries_med_iqr_us$work_year)), 
                     labels = c("2020", "2021", "2022")) + 
  labs(title = "Median Data Science Salary (USD) over 2020 - 2022, US-based companies", 
       subtitle = "Shaded area represents interquartile range (Q1 to Q3)", 
       x = "Year", y = "Median Salary (USD)")


# (4) Salary differences by experience level ------------------------------

# Create legend description
exper_legend <- c("EN" = "Entry-level / Junior", 
                   "MI" = "Mid-level / Intermediate", 
                   "SE" = "Senior-level / Expert", 
                   "EX" = "Executive-level / Director")

# Order the levels of experience_level
salaries_df$experience_level <- factor(salaries_df$experience_level, 
                                       levels = c("EN", "MI", "SE", "EX"))

# Group by experience_level and calculate average salary for each level
average_salary_per_level <- salaries_df %>%
  group_by(experience_level) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_level)

# Create the plot
ggplot(salaries_df, aes(x = experience_level, y = salary_in_usd, 
                        fill = experience_level)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  labs(title = "Data Science Salary by Experience Level", 
       x = "Experience Level", y = "Salary (USD)") + 
  scale_fill_manual(values = c("EN" = "red", "MI" = "blue", "SE" = "green", "EX" = "purple"), 
                    labels = exper_legend) + 
  guides(fill = guide_legend(title = "Experience Level Descriptions"))

# Create the same plot but for US-based companies only
salaries_df_us$experience_level <- factor(salaries_df_us$experience_level, 
                                       levels = c("EN", "MI", "SE", "EX"))

average_salary_per_level_us <- salaries_df_us %>%
  group_by(experience_level) %>%
  summarise(average_salary = mean(salary_in_usd))

print(average_salary_per_level_us)

ggplot(salaries_df_us, aes(x = experience_level, y = salary_in_usd, 
                        fill = experience_level)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  labs(title = "Data Science Salary by Experience Level", 
       x = "Experience Level", y = "Salary (USD)") + 
  scale_fill_manual(values = c("EN" = "red", "MI" = "blue", "SE" = "green", "EX" = "purple"), 
                    labels = exper_legend) + 
  guides(fill = guide_legend(title = "Experience Level Descriptions"))


# (5) Salary variations by employment type ------------------------------------

# Create legend description
employ_legend <- c("PT" = "Part-time", 
                  "FT" = "Full-time", 
                  "CT" = "Contract", 
                  "FL" = "Freelance")

# Order the levels of employment type
salaries_df$employment_type <- factor(salaries_df$employment_type, 
                                       levels = c("PT", "FT", "CT", "FL"))

# Group by employment type and calculate average salary for each level
average_salary_per_employ <- salaries_df %>%
  group_by(employment_type) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_employ)

# Create the plot
ggplot(salaries_df, aes(x = employment_type, y = salary_in_usd, fill = employment_type)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary by Employment Type, all companies", 
       x = "Employment Type", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_fill_manual(values = c("PT" = "red", "FT" = "blue", "CT" = "green", "FL" = "purple"), 
                    labels = employ_legend) + 
  guides(fill = guide_legend(title = "Employment Type Descriptions"))

# Create the same plot but for US-based companies only
salaries_df_us$employment_type <- factor(salaries_df_us$employment_type, 
                                      levels = c("PT", "FT", "CT", "FL"))

average_salary_per_employ_us <- salaries_df_us %>%
  group_by(employment_type) %>%
  summarise(average_salary = mean(salary_in_usd))

print(average_salary_per_employ_us)

ggplot(salaries_df_us, aes(x = employment_type, y = salary_in_usd, fill = employment_type)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary by Employment Type, US-based companies only", 
       x = "Employment Type", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_fill_manual(values = c("PT" = "red", "FT" = "blue", "CT" = "green", "FL" = "purple"), 
                    labels = employ_legend) + 
  guides(fill = guide_legend(title = "Employment Type Descriptions"))


# (6) Salary comparison between offshore and the United States ----------------

# Create location_type column based on whether the position is US or Offshore
salaries_df$location_type <- ifelse(salaries_df$employee_residence == "US", "US", "Offshore")

# Group by location type and calculate average salary for each level
average_salary_per_location <- salaries_df %>%
  group_by(location_type) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_location)

# Create the plot
ggplot(salaries_df, aes(x = location_type, y = salary_in_usd, fill = location_type)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary Comparison - Offshore vs. US, all companies", 
       x = "Employee Residence", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format()) + 
  guides(fill = guide_legend(title = "Location Type"))

# Create the same plot but with only US-based companies
salaries_df_us$location_type <- ifelse(salaries_df_us$employee_residence == "US", "US", "Offshore")

average_salary_per_location_us <- salaries_df_us %>%
  group_by(location_type) %>%
  summarise(average_salary = mean(salary_in_usd))

print(average_salary_per_location_us)

ggplot(salaries_df_us, aes(x = location_type, y = salary_in_usd, fill = location_type)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary Comparison - Offshore vs. US, US-based companies only", 
       x = "Employee Residence", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format()) + 
  guides(fill = guide_legend(title = "Location Type"))


# (7) Salary and remote work ratio --------------------------------------------

# Group by remote_ratio and calculate average salary for each level
average_salary_per_remote <- salaries_df %>%
  group_by(factor(remote_ratio)) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_remote)

# Create the plot
ggplot(salaries_df, aes(x = factor(remote_ratio), y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Salary and Remote Work Ratio, all companies", 
       x = "Remote Work Ratio (%)", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format())

# Create the same plot but for US-based companies only
average_salary_per_remote_us <- salaries_df_us %>%
  group_by(factor(remote_ratio)) %>%
  summarise(average_salary = mean(salary_in_usd))

print(average_salary_per_remote_us)

ggplot(salaries_df_us, aes(x = factor(remote_ratio), y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Salary and Remote Work Ratio, US-based companies", 
       x = "Remote Work Ratio (%)", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format())


# (8) Salary and company size -------------------------------------------------

# Order S, M, L companies
salaries_df$company_size <- factor(salaries_df$company_size, 
                                   levels = c("S", "M", "L"))

# Group by company size and calculate average salary for each level
average_salary_per_size <- salaries_df %>%
  group_by(company_size) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_size)

# Create the plot
ggplot(salaries_df, aes(x = company_size, y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary by Company Size, all companies", 
       x = "Company Size", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format())

# Create the same plot but for US-based companies only
salaries_df_us$company_size <- factor(salaries_df_us$company_size, 
                                      levels = c("S", "M", "L"))

average_salary_per_size_us <- salaries_df_us %>%
  group_by(company_size) %>%
  summarise(average_salary = mean(salary_in_usd))

print(average_salary_per_size_us)

ggplot(salaries_df_us, aes(x = company_size, y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary by Company Size, US-based companies only", 
       x = "Company Size", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format())


# (9) Salary comparison by position title ---------------------------------

# Extract keywords
keyword_map <- c("Analyst", "Scientist", "Engineer", "Consultant", "Researcher")
regex_keywords <- paste(keyword_map, collapse = "|")

# Create a custom function to extract keywords
extract_keyword <- function(title) {
  matching_keyword <- str_extract(title, regex(regex_keywords, ignore_case = TRUE))
  if (!is.na(matching_keyword)) {
    matching_keyword
  } else {
    "Other"
  }
}

# Mutate title_keywords using the custom function
salaries_df <- salaries_df %>% 
  mutate(title_keywords = sapply(job_title, extract_keyword))

# Filter rows where title_keywords is not "Other"
salaries_df_filtered <- salaries_df %>% 
  filter(title_keywords != "Other")

# Group by title_keywords and calculate average salary for each level
average_salary_per_title <- salaries_df_filtered %>%
  group_by(title_keywords) %>%
  summarise(average_salary = mean(salary_in_usd))

# Print the table
print(average_salary_per_title)

# Create the plot
ggplot(salaries_df_filtered, aes(x = title_keywords, y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Data Science Salary by Position Title", 
       x = "Position Title", y = "Salary (USD)") + 
  scale_y_continuous(labels = scales::comma_format())
