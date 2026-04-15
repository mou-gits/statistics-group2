# ============================================
# Housing Affordability Project - analysis.R
# ============================================

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)

# --------------------------------------------
# 0. CONFIG
# --------------------------------------------
apartment_label <- "1 Bedroom"
apartment_type_file <- gsub("[^A-Za-z0-9]+", "_", apartment_label)

# structural break year
break_year <- 2022

# --------------------------------------------
# 1. LOAD DATA
# --------------------------------------------
rent_data <- read.csv("data/clean_rent_data.csv", stringsAsFactors = FALSE)

# optional CPI file:
# expected columns: year, CPI
# place it at data/cpi.csv
cpi_available <- file.exists("data/cpi.csv")

if (cpi_available) {
  cpi_data <- read.csv("data/cpi.csv", stringsAsFactors = FALSE, check.names = FALSE)
  
  names(cpi_data) <- trimws(names(cpi_data))
  
  print("CPI column names found:")
  print(names(cpi_data))
  
  if (!("year" %in% names(cpi_data)) && ncol(cpi_data) >= 1) {
    names(cpi_data)[1] <- "year"
  }
  
  if (!("CPI" %in% names(cpi_data)) && ncol(cpi_data) >= 2) {
    names(cpi_data)[2] <- "CPI"
  }
  
  cpi_data$year <- as.numeric(cpi_data$year)
  cpi_data$CPI  <- as.numeric(cpi_data$CPI)
  
  cpi_data <- cpi_data %>%
    filter(!is.na(year), !is.na(CPI))
  
  print("Cleaned CPI data:")
  print(cpi_data)
  
} else {
  cpi_data <- NULL
}

# --------------------------------------------
# 2. BASIC CLEANUP
# --------------------------------------------
rent_data <- rent_data %>%
  mutate(
    city = str_trim(city),
    year = as.numeric(year),
    rent = as.numeric(rent),
    university_flag = as.numeric(university_flag),
    university_type = ifelse(university_flag == 1, "University", "Non-University")
  ) %>%
  filter(!is.na(city), !is.na(year), !is.na(rent))

# create output folders if missing
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/plots")) dir.create("output/plots", recursive = TRUE)

# --------------------------------------------
# 3. SUMMARY TABLES
# --------------------------------------------

overall_summary <- rent_data %>%
  summarise(
    apartment_type = apartment_label,
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_rows = n(),
    n_cities = n_distinct(city),
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    min_rent = min(rent, na.rm = TRUE),
    max_rent = max(rent, na.rm = TRUE)
  )

print("Overall Summary")
print(overall_summary)

year_summary <- rent_data %>%
  group_by(year) %>%
  summarise(
    apartment_type = first(apartment_label),
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    min_rent = min(rent, na.rm = TRUE),
    max_rent = max(rent, na.rm = TRUE),
    .groups = "drop"
  )

print("Year Summary")
print(year_summary)

city_summary <- rent_data %>%
  group_by(city, university_type) %>%
  summarise(
    apartment_type = first(apartment_label),
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    min_rent = min(rent, na.rm = TRUE),
    max_rent = max(rent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_rent))

print("City Summary")
print(city_summary)

university_summary <- rent_data %>%
  group_by(university_type) %>%
  summarise(
    apartment_type = first(apartment_label),
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    min_rent = min(rent, na.rm = TRUE),
    max_rent = max(rent, na.rm = TRUE),
    .groups = "drop"
  )

print("University vs Non-University Summary")
print(university_summary)

write.csv(overall_summary, paste0("output/", apartment_type_file, "_overall_summary.csv"), row.names = FALSE)
write.csv(year_summary, paste0("output/", apartment_type_file, "_year_summary.csv"), row.names = FALSE)
write.csv(city_summary, paste0("output/", apartment_type_file, "_city_summary.csv"), row.names = FALSE)
write.csv(university_summary, paste0("output/", apartment_type_file, "_university_summary.csv"), row.names = FALSE)

# --------------------------------------------
# 4. GROWTH CALCULATIONS
# --------------------------------------------
city_growth <- rent_data %>%
  group_by(city, university_type, university_flag) %>%
  summarise(
    apartment_type = first(apartment_label),
    rent_start = rent[year == min(year)][1],
    rent_end = rent[year == max(year)][1],
    avg_rent = mean(rent, na.rm = TRUE),
    growth_abs = rent_end - rent_start,
    growth_pct = ((rent_end - rent_start) / rent_start) * 100,
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(growth_pct))

print("City Growth")
print(city_growth)

write.csv(city_growth, paste0("output/", apartment_type_file, "_city_growth.csv"), row.names = FALSE)

overall_growth <- year_summary %>%
  summarise(
    apartment_type = first(apartment_label),
    avg_rent_start = mean_rent[year == min(year)],
    avg_rent_end = mean_rent[year == max(year)],
    growth_abs = avg_rent_end - avg_rent_start,
    growth_pct = ((avg_rent_end - avg_rent_start) / avg_rent_start) * 100
  )

print("Overall Growth")
print(overall_growth)

write.csv(overall_growth, paste0("output/", apartment_type_file, "_overall_growth.csv"), row.names = FALSE)

# --------------------------------------------
# 5. STATISTICAL TESTS
# --------------------------------------------

uni_test <- t.test(rent ~ university_type, data = rent_data)
print("T-test: University vs Non-University")
print(uni_test)

capture.output(
  uni_test,
  file = paste0("output/", apartment_type_file, "_t_test_university_vs_nonuniversity.txt")
)

rent_data <- rent_data %>%
  mutate(period_group = ifelse(year < break_year, "Before Break", "After Break"))

break_test <- t.test(rent ~ period_group, data = rent_data)
print(paste("T-test: Before vs After", break_year))
print(break_test)

capture.output(
  break_test,
  file = paste0("output/", apartment_type_file, "_t_test_structural_break.txt")
)

# --------------------------------------------
# 6. VISUALIZATIONS
# --------------------------------------------

p1 <- ggplot(year_summary, aes(x = year, y = mean_rent)) +
  geom_line() +
  geom_point() +
  labs(
    title = paste("Average", apartment_label, "Rent Over Time"),
    x = "Year",
    y = "Average Rent ($)"
  ) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_avg_rent_over_time.png"),
  plot = p1, width = 8, height = 5
)

p2 <- ggplot(rent_data, aes(x = year, y = rent, group = city, color = university_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = paste(apartment_label, "Rent Trends by City"),
    x = "Year",
    y = "Rent ($)",
    color = "City Type"
  ) +
  scale_color_manual(values = c("University" = "pink", "Non-University" = "lightgreen")) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_city_trends.png"),
  plot = p2, width = 10, height = 6
)

p3 <- ggplot(rent_data, aes(x = factor(year), y = rent)) +
  geom_boxplot() +
  labs(
    title = paste("Distribution of", apartment_label, "Rents by Year"),
    x = "Year",
    y = "Rent ($)"
  ) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_boxplot_by_year.png"),
  plot = p3, width = 8, height = 5
)

p4 <- ggplot(rent_data, aes(x = university_type, y = rent, fill = university_type)) +
  geom_boxplot() +
  labs(
    title = paste(apartment_label, "Rents: University vs Non-University Cities"),
    x = "City Type",
    y = "Rent ($)",
    fill = "City Type"
  ) +
  scale_fill_manual(values = c("University" = "pink", "Non-University" = "lightgreen")) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_university_vs_nonuniversity_boxplot.png"),
  plot = p4, width = 8, height = 5
)

year_uni_summary <- rent_data %>%
  group_by(year, university_type) %>%
  summarise(mean_rent = mean(rent, na.rm = TRUE), .groups = "drop")

p_uni_trend <- ggplot(year_uni_summary, aes(x = year, y = mean_rent, color = university_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = paste(apartment_label, "Average Rent Over Time: University vs Non-University"),
    x = "Year",
    y = "Average Rent ($)",
    color = "City Type"
  ) +
  scale_color_manual(values = c("University" = "pink", "Non-University" = "lightgreen")) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_university_vs_nonuniversity_trend.png"),
  plot = p_uni_trend, width = 8, height = 5
)

p5 <- ggplot(city_growth, aes(x = avg_rent, y = growth_pct, size = n_obs, label = city, color = university_type)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -0.8, size = 3) +
  labs(
    title = paste(apartment_label, "Bubble Plot: Average Rent vs Rent Growth"),
    x = "Average Rent ($)",
    y = "Growth from Start to End (%)",
    size = "Observations",
    color = "City Type"
  ) +
  scale_color_manual(values = c("University" = "pink", "Non-University" = "lightgreen")) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_bubble_plot.png"),
  plot = p5, width = 10, height = 6
)

p6 <- ggplot(city_summary, aes(x = reorder(city, mean_rent), y = mean_rent, fill = university_type)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Average", apartment_label, "Rent by City"),
    x = "City",
    y = "Average Rent ($)",
    fill = "City Type"
  ) +
  scale_fill_manual(values = c("University" = "pink", "Non-University" = "lightgreen")) +
  theme_minimal()

ggsave(
  paste0("output/plots/", apartment_type_file, "_avg_rent_by_city.png"),
  plot = p6, width = 9, height = 6
)

# --------------------------------------------
# 7. CPI COMPARISON (OPTIONAL)
# --------------------------------------------
if (!is.null(cpi_data)) {
  
  rent_cpi <- year_summary %>%
    select(year, mean_rent) %>%
    inner_join(cpi_data, by = "year") %>%
    arrange(year) %>%
    mutate(
      rent_index = mean_rent / mean_rent[1],
      cpi_index = CPI / CPI[1]
    )
  
  write.csv(rent_cpi, paste0("output/", apartment_type_file, "_rent_vs_cpi_index.csv"), row.names = FALSE)
  
  p7 <- ggplot(rent_cpi, aes(x = year)) +
    geom_line(aes(y = rent_index, linetype = "Rent")) +
    geom_point(aes(y = rent_index)) +
    geom_line(aes(y = cpi_index, linetype = "CPI")) +
    geom_point(aes(y = cpi_index)) +
    labs(
      title = paste("Indexed", apartment_label, "Rent vs CPI (Base Year = 1)"),
      x = "Year",
      y = "Index",
      linetype = "Series"
    ) +
    theme_minimal()
  
  ggsave(
    paste0("output/plots/", apartment_type_file, "_rent_vs_cpi.png"),
    plot = p7, width = 8, height = 5
  )
  
  print("CPI comparison completed.")
  
} else {
  print("No CPI file found at data/cpi.csv. CPI analysis skipped.")
}

# --------------------------------------------
# 8. QUICK TEXT OUTPUT FOR REPORT
# --------------------------------------------
highest_city <- city_summary %>% slice(1)
lowest_city <- city_summary %>% slice(n())

report_notes <- c(
  paste("The dataset covers", min(rent_data$year), "to", max(rent_data$year), "for", n_distinct(rent_data$city), "selected Ontario CMAs."),
  paste("The analysis focuses on", apartment_label, "apartment rents."),
  paste("The overall mean rent is", round(overall_summary$mean_rent, 2), "dollars."),
  paste("The highest average rent was observed in", highest_city$city, "at", round(highest_city$mean_rent, 2), "dollars."),
  paste("The lowest average rent was observed in", lowest_city$city, "at", round(lowest_city$mean_rent, 2), "dollars."),
  paste("Average", apartment_label, "rent changed by", round(overall_growth$growth_pct, 2), "percent over the study period."),
  paste("The t-test comparing university and non-university cities produced a p-value of", signif(uni_test$p.value, 4), "."),
  paste("The structural break test at", break_year, "produced a p-value of", signif(break_test$p.value, 4), ".")
)

writeLines(report_notes, paste0("output/", apartment_type_file, "_report_notes.txt"))
print(report_notes)

# --------------------------------------------
# 9. DONE
# --------------------------------------------
print("Analysis complete.")
print("Check output/ for tables, report notes, and plots.")