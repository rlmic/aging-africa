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
    coord_cartesian(clip = "off") +
    labs(title = title, y = "", x = "") +
    custom_theme +
    add_curves()
}

# Define a reusable function for dependency ratio plots
create_dependency_ratio_plot <- function(
    data, title, line_sizes, color_scheme, 
    line_types = c("dotted", "dashed", "solid"),
    y_labels = c("0", "", "0.5", "", "1", "", "1.50"),
    limits = c(0, 1.5), segment_curvature = -0.1,
    breaks_numer, breaks_text, include_country_labels = TRUE
) {
  
  plot <- ggplot(data, aes(x = year, y = ratio, group = countryregion)) +
    geom_line(aes(color = countryregion, size = countryregion, linetype = countryregion)) +
    scale_size_manual(values = line_sizes) +
    scale_linetype_manual(values = line_types) +
    scale_color_manual(values = color_scheme) +
    coord_cartesian(clip = "off") +
    labs(x = "", y = "", title = title) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = "dotted"),
      axis.text.y = element_blank(),
      text = element_text(size = 20, color = "#344771"),
      plot.title = element_markdown(size = 20, color = "#344771", hjust = 0.5),
      axis.text = element_markdown(size = 20, margin = margin(5, 0, 0, 0)),
      plot.background = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(1, 20, 0, 0),
      legend.position = "none",
      axis.line.x = element_line(size = 0.5, colour = "#344771", linetype = 1)
    ) +
    scale_x_continuous(breaks = breaks_numer, labels = breaks_text) +
    scale_y_continuous(limits = limits, breaks = seq(limits[1], limits[2], 0.25), labels = y_labels, expand = c(0, 0))
  
  if (include_country_labels) {
    plot <- plot +
      geom_text_repel(
        aes(color = countryregion, label = name_lab),
        segment.angle = 10,
        size = 7.5,
        direction = "y",
        hjust = 0,
        vjust = 1.7,
        segment.size = .4,
        segment.alpha = .5,
        segment.linetype = "dotted",
        box.padding = .2,
        segment.curvature = segment_curvature
      ) +
      geom_text_repel(
        aes(color = countryregion, label = ratio),
        segment.angle = 20,
        size = 6,
        direction = "y",
        hjust = 0,
        vjust = -1.8,
        segment.size = 1.1,
        segment.alpha = .5,
        segment.linetype = "dotted",
        box.padding = .4,
        segment.curvature = segment_curvature
      )
  } else {
    plot <- plot +
      geom_text_repel(
        aes(color = countryregion, label = ratio),
        segment.angle = 20,
        size = 6,
        direction = "y",
        hjust = 0,
        vjust = -1.8,
        segment.size = 1.1,
        segment.alpha = .5,
        segment.linetype = "dotted",
        box.padding = .4,
        segment.curvature = segment_curvature
      )
  }
  
  return(plot)
}


# Helper function to recode values based on a mapping
recode_values <- function(df, column, mapping) {
  df[[column]] <- recode(df[[column]], !!!mapping)
  return(df)
}

# Function to process each income group
process_income_group <- function(data, country_filter) {
  data %>%
    filter(str_detect(country, country_filter)) %>% 
    select(-cou) %>%
    gather(variable, value, physicians:uhcidx, factor_key = TRUE)
}

# Function to prepare long format data and assign country-specific order
prep_data_long <- function(data, countries_filter) {
  data %>%
    filter(country %in% countries_filter) %>%
    select(-cou) %>%
    gather(
      variable, 
      value, 
      govtexpgdp:physicians, 
      factor_key = TRUE
      ) %>%
    mutate(order = ifelse(
      country %in% countries_filter, 
      match(country, countries_filter),
      row_number())) %>%
    arrange(variable, order)
}

trans_input <- function(data, var_vec, ids, var_nam="condition", val_nam="measurement", recoding_map=NULL, group_vars=NULL) {
  data <- data %>%
    mutate(
      age_group5 = as.factor(age_group5),
      work_hrs = work_hrs / 40
    ) %>%
    melt(id.vars = ids, measure.vars = var_vec, variable.name = var_nam, value.name = val_nam) %>%
    filter(!age_group5 %in% c('0', '5', '10', '15'))
  
  if (!is.null(recoding_map)) {
    data <- recode_values(data, names(recoding_map)[1], recoding_map[[1]])
  }
  
  if (!is.null(group_vars)) {
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarize(!!val_nam := mean(get(val_nam), na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
  }
  
  return(data)
}

