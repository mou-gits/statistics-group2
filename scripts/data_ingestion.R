library(readxl)
library(dplyr)
library(stringr)

# -----------------------------
# CONFIG
# -----------------------------

base_path <- "rawdata/Excel files for project/Ontario"
years <- 2019:2025

# 🔥 City pattern (you can change this later)
city_pattern <- "Kitchener"

# -----------------------------
# FUNCTION: Find correct sheet
# -----------------------------

find_rent_sheet <- function(file_path) {
  
  sheets <- excel_sheets(file_path)
  
  for (sheet in sheets) {
    
    df_preview <- read_excel(file_path, sheet = sheet, n_max = 3, col_names = FALSE)
    
    text_blob <- paste(unlist(df_preview), collapse = " ")
    
    if (str_detect(tolower(text_blob), "average rent") &&
        str_detect(tolower(text_blob), "apartment")) {
      
      return(sheet)
    }
  }
  
  stop(paste("No rent sheet found in", file_path))
}

# -----------------------------
# FUNCTION: Extract rent column
# -----------------------------

extract_rent_column <- function(df) {
  
  colnames_clean <- tolower(colnames(df))
  
  # Try Bachelor first
  bachelor_idx <- grep("bachelor", colnames_clean)
  
  if (length(bachelor_idx) > 0) {
    return(df[[bachelor_idx[1]]])
  }
  
  # Fallback: 1-bedroom
  onebed_idx <- grep("1", colnames_clean)
  
  if (length(onebed_idx) > 0) {
    return(df[[onebed_idx[1]]])
  }
  
  stop("No suitable rent column found")
}

# -----------------------------
# MAIN LOOP
# -----------------------------

all_data <- data.frame()

for (year in years) {
  
  file_path <- paste0(base_path, "/cmhc_rms_ontario_", year, ".xlsx")
  
  print(paste("Processing year:", year))
  
  # 🔍 Find correct sheet dynamically
  sheet_name <- find_rent_sheet(file_path)
  
  print(paste("Using sheet:", sheet_name))
  
  df <- read_excel(file_path, sheet = sheet_name)
  
  # Rename first column to Centre
  colnames(df)[1] <- "Centre"
  
  # 🔥 Filter for Kitchener (pattern-based)
  df_filtered <- df %>%
    filter(str_detect(Centre, city_pattern))
  
  print("Filtered rows:")
  print(df_filtered)
  
  if (nrow(df_filtered) == 0) {
    warning(paste("No rows found for year", year))
    next
  }
  
  # Extract rent column
  rent_values <- extract_rent_column(df_filtered)
  
  temp <- df_filtered %>%
    mutate(
      rent = as.numeric(rent_values),
      year = year
    ) %>%
    select(Centre, year, rent)
  
  all_data <- bind_rows(all_data, temp)
}

# -----------------------------
# FINAL OUTPUT
# -----------------------------

print("Final dataset:")
print(all_data)

print(paste("Total rows:", nrow(all_data)))

# Save
write.csv(all_data, "data/kitchener_test.csv", row.names = FALSE)