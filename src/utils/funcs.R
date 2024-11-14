# Helper function to load datasets
load_data <- function(file_name) {
  read_dta(file.path(data_path, file_name))
}

# Function to handle adding "Rest of World" row if data has at least 4 rows
add_rest_of_world <- function(data, prefix) {
  if (nrow(data) >= 4) {
    rest_of_world_values <- as.numeric(as.vector(t(data[4, 2:5]))) - 
      as.numeric(as.vector(t(data[3, 2:5]))) - 
      as.numeric(as.vector(t(data[2, 2:5]))) - 
      as.numeric(as.vector(t(data[1, 2:5])))
    rest_of_world_row <- c("Rest of World", rest_of_world_values)
    data <- rbind(data, rest_of_world_row)
  }
  data
}

# Function to reshape and clean data
reshape_and_clean <- function(data, prefix, keepobs, order) {
  data %>%
    gather("condition", "measurement", starts_with(prefix), factor_key = TRUE) %>%
    filter(countryregion != 'World') %>%
    mutate(
      condition = as.numeric(str_remove_all(condition, paste0(prefix, "_"))),
      measurement = as.numeric(measurement),
      countryregion = factor(countryregion, levels = order)
    )
}

# Processing dep_rat_ov60 and dep_rat_ov65 with additional transformation
process_dependency_ratio <- function(data, prefix, countries) {
  data %>%
    filter(countryregion %in% countries) %>%
    select(countryregion, starts_with(prefix)) %>%
    gather("year", "ratio", starts_with(prefix), factor_key = TRUE) %>%
    mutate(year = as.numeric(str_remove_all(year, prefix))) %>%
    group_by(countryregion) %>%
    mutate(
      name_lab = if_else(year == 2100, countryregion, NA_character_),
      name_lab = if_else(name_lab == 'Sub-Saharan Africa', 'Sub-Saharan Africa\n         Average', name_lab)
    ) %>%
    ungroup() %>%
    mutate(ratio = round(as.numeric(ratio), 2))
}


# Helper function for cent calculation
cent <- function(dat, sel) {
  dat <- dat[order(dat$countryregion), ]
  tot <- dat %>%
    filter(condition == 2100) %>%
    tail(sel) %>%
    summarise(total = sum(measurement)) %>%
    pull(total)
  
  rest <- dat %>%
    filter(condition == 2100) %>%
    tail(sel) %>%
    head(1) %>%
    summarise(rest = sum(measurement) / 2) %>%
    pull(rest)
  
  num <- tot - rest
  return(num)
}

# Helper function for total size calculation
end_val <- function(dat, year) {
  size <- dat %>%
    filter(condition == year) %>%
    summarise(total = sum(measurement)) %>%
    pull(total)
  return(size)
}

# Common annotation function to avoid repetition
add_annotation <- function(x, y, label, color, size) {
  annotate("text", x = x, y = y, label = label, hjust = 0, color = color, size = size)
}

# Common segment function to avoid repetition
add_segment <- function(x, yend) {
  geom_segment(aes(x = x, xend = x, y = 0, yend = yend), color = "black", size = 0.3)
}
