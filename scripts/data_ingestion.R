library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# -----------------------------
# CONFIG
# -----------------------------
file_path <- "rawdata/cleaned-ontario.xlsx"
rent_type <- "Bachelor"   # change if needed

# exact city list we decided
cities_keep <- c(
  "Toronto CMA",
  "Ottawa-Gatineau CMA (Ont. part)",
  "Kitchener-Cambridge-Waterloo CMA",
  "London CMA",
  "Barrie CMA",
  "Windsor CMA",
  "Thunder Bay CMA",
  "Greater Sudbury/Grand Sudbury CMA"
)

# university cities from that list
university_cities <- c(
  "Toronto CMA",
  "Ottawa-Gatineau CMA (Ont. part)",
  "Kitchener-Cambridge-Waterloo CMA",
  "London CMA"
)

# -----------------------------
# READ ALL SHEETS
# -----------------------------
years <- excel_sheets(file_path)

all_data <- data.frame()

for (yr in years) {
  
  message("Reading sheet: ", yr)
  
  df <- read_excel(file_path, sheet = yr, col_names = TRUE)
  
  # rename first column
  colnames(df)[1] <- "city"
  
  # clean names
  colnames(df) <- str_trim(colnames(df))
  
  # validate column exists
  if (!(rent_type %in% colnames(df))) {
    stop(paste("Column not found in sheet", yr, ":", rent_type))
  }
  
  temp <- df %>%
    select(city, rent = all_of(rent_type)) %>%
    mutate(
      city = str_trim(as.character(city)),
      rent = as.character(rent),
      year = as.numeric(yr)
    )
  
  all_data <- bind_rows(all_data, temp)
}

# -----------------------------
# CLEAN + FILTER
# -----------------------------
clean_data <- all_data %>%
  mutate(
    rent = ifelse(rent == "**", NA, rent),
    rent = str_replace_all(rent, ",", ""),
    rent = as.numeric(rent)
  ) %>%
  filter(city %in% cities_keep) %>%
  mutate(
    university_flag = ifelse(city %in% university_cities, 1, 0)
  ) %>%
  arrange(year, city)

# -----------------------------
# SAVE
# -----------------------------
write.csv(clean_data, "data/clean_rent_data.csv", row.names = FALSE)

# -----------------------------
# CHECKS
# -----------------------------
print(clean_data)
print(table(clean_data$year))
print(unique(clean_data$city))
print(summary(clean_data$rent))