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


pop_row <- function(
    forma_theme,
    colors = colors_gende,
    breaks = bre_age,
    labels = lab_age,
    size_count = 11
) {
  ssa <- bind_rows(ssa, ssa %>% mutate(country = '', region = "Average"))
  ssa <- ssa %>%
    filter(!age_group5 %in% c('0', '5', '10', '15')) %>%
    filter(age >= 20 & !is.na(gender)) %>% 
    mutate(gender = factor(gender, levels = c("Female", "Male")))
  
  graph <- ggplot(ssa, aes(x = age_group5, fill = gender)) +
    theme_minimal() +
    geom_bar(binwidth = 0.5, color = "white", position = "dodge") +
    facet_nested(
      ~ factor(region, levels = regions) + country, 
      scales = 'free_y', space = "free_y", 
      nest_line = element_line(linetype = 1)
    ) +
    force_panelsizes(cols = c(1, rep(0.85, 7))) +
    scale_fill_manual("legend", values = colors) +
    geom_text(
      data = get_count("gender"), 
      aes(label = n, y = Inf, x = -Inf),
      vjust = 1,
      colour = "#14213D",
      hjust = -0.2,
      size = size_count
    ) +
    forma_theme
  
  return(graph)
}



fac_row <- function(
    var,
    forma_theme,
    weighted = TRUE,
    colors = colors_gende,
    size_count = 12
) {
  # Common function to process aggregate data
  process_aggregate <- function(data, var_col) {
    data %>%
      mutate(
        vari = get(var_col),
        female = case_when(
          female == 0 ~ 'Male',
          female == 1 ~ 'Female',
          TRUE ~ NA
        ),
        work_hrs = case_when(
          work_hrs >= 140 & work_hrs <= 168 ~ 140,
          work_hrs > 168 ~ NA,
          TRUE ~ work_hrs
        ),
        gender = as.factor(female),
        age_group5 = as.factor(age_group5),
        country = '',
        region = 'Average'
      ) %>%
      filter(!is.na(gender), !age_group5 %in% c('0', '5', '10', '15'))
  }
  
  # Process main and aggregate datasets
  data <- ssa %>%
    filter(age >= 20 & !is.na(gender)) %>%
    group_by(age_group5, country, gender, region) %>%
    summarise(
      vari = weighted.mean(get(var), weights_survey, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(age_group5 = as.factor(age_group5))
  
  aggregate_data <- if (weighted) {
    process_aggregate(ssa_agg, var)
  } else {
    process_aggregate(ssa_agg_non, var)
  }
  
  # Combine datasets
  data <- bind_rows(data, aggregate_data)
  
  # Ensure consistent factor levels for gender
  data$gender <- factor(data$gender, levels = c("Female", "Male"))
  
  # Get count data for labels
  dat <- get_count(var) %>%
    filter(n !="N=0")  # Exclude rows where N=0
  
  # Generate the plot
  graph <- ggplot(
    data,
    aes(x = age_group5, y = vari, colour = gender, group = gender)
  ) +
    theme_minimal() +
    geom_line(size = 3, aes(linetype = gender)) +
    facet_nested(
      ~ factor(region, levels = regions) + country,
      scales = 'free_y',
      space = "free_y"
    ) +
    scale_color_manual(values = colors) +
    force_panelsizes(cols = c(1, rep(0.85, 7))) +
    scale_linetype_manual(values = c("Female" = "solid", "Male" = "longdash")) +
    geom_text(
      data = dat,  # Use filtered data
      aes(label = n, y = Inf, x = -Inf),
      vjust = 1,
      colour = "#14213D",
      hjust = -0.2,
      size = size_count
    ) +
    geom_rect(
      aes(xmin = "60", xmax = Inf, ymin = -Inf, ymax = Inf),
      alpha = 0.005,
      fill = "#8eb67d"
    ) +
    forma_theme +
    guides(
      linetype = "none",  # Hide the linetype legend
      colour = guide_legend(
        title = "Gender",
        override.aes = list(linetype = c("Female" = "solid", "Male" = "longdash"))
      )
    )
  
  return(graph)
}

cond_fac_row <- function(data_col, y_label, weighted = TRUE, breaks_y, limits_y, labels_y, breaks_x, labels_x, x_pos = "bottom", theme = theme_middle()) {
  # Special case for 'work_hrs'
  if (data_col == "work_hrs") {
    y_scale <- scale_y_continuous(
      breaks = c(0, 20, 40), 
      limits = c(0, 50), 
      labels = c('0', '-', '40')
    )
  } else {
    y_scale <- scale_y_continuous(
      breaks = breaks_y,
      limits = limits_y,
      labels = labels_y
    )
  }
  
  fac_row(data_col, theme, weighted) + 
    labs(
      y = y_label, 
      x = "Age"
    ) + 
    scale_x_discrete(
      breaks = breaks_x,
      labels = labels_x,
      position = x_pos
    ) + 
    y_scale  # Apply the appropriate y-scale
}


meas_count_gend <- function(weighted = TRUE, breaks_y = c(0, 0.5, 1), limits_y = c(0, 1.2), labels_y = c('0', '-', '1'), breaks_x = bre_age, labels_x = lab_age,
                                data_cols, y_labels, order_first = "pop_row", custom_plots = NULL, 
                                themes = list()) {
  
  # Create the individual plots dynamically
  plots <- list()  # This will hold all the plots
  rel_heights <- c()  # To store relative heights dynamically
  labels_prefix <- LETTERS  # Start label letters
  
  # If 'pop_row' is NOT included, start labels from 'A' for the first variable
  if (order_first != "pop_row") {
    for (i in 1:length(data_cols)) {
      # Determine the theme for the current variable
      theme <- themes[[data_cols[i]]] %||% theme_middle()  # Use the theme for the variable or default
      
      # Create each plot using the create_plot function and store it in the plots list
      plots[[i]] <- cond_fac_row(data_cols[i], y_labels[i], weighted, breaks_y, limits_y, labels_y, breaks_x, labels_x, theme = theme)
      
      # Adjust relative heights based on the theme
      if (identical(theme, theme_bottom())) {
        rel_heights <- c(rel_heights, 7.5)  # Slightly bigger for 'theme_bottom'
      } else {
        rel_heights <- c(rel_heights, 5)  # Default height
      }
    }
  } else {
    # If 'pop_row' is included, add it as the first plot
    a <- pop_row(theme_top()) + 
      labs(y = "**Observations<br>(Count)**", face = 'bold') +
      scale_x_discrete(
        breaks = breaks_x,
        labels = labels_x,
        position = "bottom"
      ) + 
      scale_y_continuous(
        breaks = c(0, 20000, 40000), 
        limits = c(0, 50000), 
        labels = c('0', '20K', '40K')
      )
    plots <- c(list(a), plots)
    rel_heights <- c(rel_heights, 9)  # Fixed height for 'pop_row'
    
    # Add the other variables starting from the second plot
    for (i in 1:length(data_cols)) {
      theme <- themes[[data_cols[i]]] %||% theme_middle()
      plots[[i + 1]] <- cond_fac_row(data_cols[i], y_labels[i], weighted, breaks_y, limits_y, labels_y, breaks_x, labels_x, theme = theme)
      
      # Adjust relative heights for each plot
      if (identical(theme, theme_bottom())) {
        rel_heights <- c(rel_heights, 7.5)  # Slightly bigger for 'theme_bottom'
      } else {
        rel_heights <- c(rel_heights, 5)  # Default height
      }
    }
    # Adjust labels to skip the first "pop_row" plot
    labels_prefix <- c("", LETTERS[1:length(data_cols)])
  }
  
  # If user provides a custom plot (e.g., Urban Status), include it
  if (!is.null(custom_plots)) {
    for (custom_plot in custom_plots) {
      plots <- c(plots, list(custom_plot))
      rel_heights <- c(rel_heights, 5)  # Default height for custom plots
    }
  }
  
  # Create the plot grid
  plot_grid(
    plotlist = plots,  # Use the dynamically created list of plots
    rel_heights = rel_heights,  # Use dynamically adjusted heights
    ncol = 1, 
    nrow = length(plots),  # Dynamically set the number of rows
    align = "v", 
    labels = paste0(labels_prefix[1:length(plots)], ".-"),  # Dynamically create labels
    label_size = 38,
    label_colour = "#14213D"
  )
}



indi_cont_regi <- function(
    vars,
    labs,
    dat = dat_long,
    high = high_inco,
    sub = sub_sahar_inco,
    low = low_middle_inco,
    size_line = 1.1
) {
  graph = dat %>% 
    filter(variable %in% vars) %>% 
    ggplot(aes(reorder(country, order), value)) + 
    geom_bar(stat = "identity", fill = "white", color = "#1F271B", width = 0.85, size = 0.5) + 
    facet_grid(~factor(variable, levels = vars), scales = "free_x", labeller = as_labeller(labs)) +
    ylab("") + 
    xlab("") + 
    coord_flip() + 
    geom_hline(
      data = sub %>% filter(variable %in% vars), 
      aes(yintercept = value, color = "Sub-Saharan African Countries", linetype = "Sub-Saharan African Countries"),
      size = size_line+0.5, color = "#8eb67d"
    ) +
    geom_hline(
      data = low %>% filter(variable %in% vars), 
      aes(yintercept = value, linetype = "Low-middle Income Countries"),
      size = size_line, color = "#686868"
    ) +
    geom_hline(
      data = high %>% filter(variable %in% vars), 
      aes(yintercept = value, color = "High Income Countries", linetype = "High Income Countries"),
      size = size_line, color = "black"
    ) +
    guides(color = guide_legend(title = "Regional\nAverages", reverse = TRUE)) +
    scale_linetype_manual(name = "Regional\nAverages", values = c(3, 2, 1), 
                          guide = guide_legend(title = "Regional\nAverages", reverse = TRUE, 
                                               override.aes = list(color = c("#8eb67d", "#686868", "black")))) +
    theme_minimal() +
    theme_broad_outc() +
    
    # Assign labels to the countries in the plot
    geom_text(
      aes(label = ifelse(country %in% countries, country, ""),
          hjust = ifelse(country == "South Africa", 0.9, -0.05)), 
      color = "#344771", 
      size = 4.5
    ) 
  
  return(graph)
}
