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


# Define reusable color palette and theme

custom_theme <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_markdown(size = 20, color = "#344771", hjust = 0.5),
    axis.text.x = element_text(size = 20, margin = margin(5, 0, 0, 0)),
    text = element_text(size = 20),
    plot.margin = unit(c(0, 0.75, 0, 0), "inches"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", size = 0.3, linetype = 2)
  )

# Define a function for repetitive curve segments
add_curves <- function() {
  list(
    geom_curve(aes(x = 1990, xend = 2019, y = -5000, yend = 4000), inherit.aes = FALSE, curvature = 0.2, color = "grey"),
    geom_curve(aes(x = 2021, xend = 2049, y = -5000, yend = 4000), inherit.aes = FALSE, curvature = 0.2, color = "grey"),
    geom_curve(aes(x = 2051, xend = 2100, y = -5000, yend = 4000), inherit.aes = FALSE, curvature = 0.2, color = "grey")
  )
}

# Define a function to encapsulate shared plot logic
create_plot <- function(data, title, y_limits, y_labels) {
  ggplot(data %>% filter(countryregion != 'Rest of World'), 
         aes(condition, measurement, fill = countryregion, label = countryregion)) +
    geom_line(aes(color = countryregion, size = countryregion, linetype = countryregion)) + 
    geom_point(aes(color = countryregion)) +
    scale_size_manual(values = c(1.4, 1.4, 1.4)) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    scale_x_continuous(breaks = c(1990, 2020, 2050, 2100), labels = c("1990", "2020", "2050", "2100")) +
    scale_y_continuous(breaks = y_limits, labels = y_labels, limits = c(0, max(y_limits))) +
    scale_color_manual(values = pal) +
    geom_label(data = . %>% mutate(
      # Adjusted placements as an example; ideally, set dynamically
      condition = case_when(
        condition %in% c(1990) ~ NA,
        condition == 2020 ~ 2005,
        condition == 2050 ~ 2035,
        condition == 2100 ~ 2075
      ),
      measurement = case_when(
        name_lab == '+ 63 %' ~ 2776948, 
        name_lab == '- 8 %' ~ 1600170,
        name_lab == '- 42 %' ~ 1039655,
        TRUE ~ measurement # Fallback to original measurement if unmatched
      )
    ), aes(label = name_lab, colour = countryregion), fill = 'white', size = 5, check_overlap = TRUE) +
    geom_text_repel(
      aes(color = countryregion, label = lab),
      segment.angle = 1,
      size = 4,
      vjust = -1,
      segment.size = 0.5,
      segment.alpha = 0.5,
      segment.linetype = "dotted",
      segment.curvature = -0.1
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title, y = "", x = "") +
    custom_theme +
    add_curves()
}

# Helper function to recode values based on a mapping
recode_values <- function(df, column, mapping) {
  df[[column]] <- recode(df[[column]], !!!mapping)
  return(df)
}