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


#' Theme for general outcomes in selected countries in Africa, which include
#' remaining life expectancy, social protection, pension coverage, health care
#' out of poclet, gov health, n of hospital beds, n of physicians


base_theme <- function(
    size_text = 18,
    size_title = 23,
    size_legend = 20,
    color = "#344771",
    grid_major = element_blank(),
    grid_minor = element_blank(),
    legend_position = "none"
) {
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, size = size_title, hjust = 1),
    axis.text = element_text(color = color, size = size_text),
    axis.text.y = element_text(color = color, size = size_text, hjust = 0),
    legend.position = legend_position,
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(
      margin = margin(t = 5, b = 5, unit = "pt"),
      size = size_legend,
      color = color
    ),
    legend.key.size = unit(6, "line"),
    legend.key = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    strip.background = element_rect(fill = "transparent", color = NA),
    strip.text = element_markdown(color = color, size = size_title, face = "bold"),
    panel.grid.major = grid_major,
    panel.grid.minor = grid_minor,
    panel.spacing = unit(5, "lines"),
    plot.margin = margin(, 2, , , "cm"),
    text = element_text(size = size_text, color = color)
  )
}

# Individual Themes

theme_broad_outc <- function(size_text = 16, size_title = 20, size_legend = 20, color = "#344771") {
  base_theme(
    size_text, size_title, size_legend, color,
    legend_position = "bottom"
  ) +
    theme(
      axis.text.y = element_blank(),
      axis.line.x.bottom = element_line(),
      axis.line.x.top = element_line(),
      panel.spacing = unit(1.5, "lines")
    )
}

theme_top_count <- function(size_text = 35, size_title = 42, size_legend = 32, color = "#344771") {
  base_theme(
    size_text, size_title, size_legend, color,
    grid_major = element_line(linetype = "dashed"),
    legend_position = "bottom"
  ) +
    theme(
      legend.justification = "right",
      panel.spacing.x = unit(5, "lines"),
      panel.spacing.y = unit(3, "lines"),
      axis.title.y = element_text(angle = 90, size = size_title, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(margin = margin(l = 20, b = 20), face = "bold")
    )
}



theme_top <- function(size_text = 35, size_title = 42, size_legend = 42, color = "#344771") {
  base_theme(
    size_text, size_title, size_legend, color,
    grid_major = element_line(linetype = "dashed")
  ) +
    theme(
      axis.ticks.x = element_blank(),
      panel.spacing.y = unit(7, "lines")
    )
}

theme_middle <- function(size_text = 35, size_title = 42, color = "#344771") {
  base_theme(
    size_text, size_title, color = color,
    grid_major = element_line(linetype = "dashed")
  ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(color = color, size = size_title, hjust = 0),
      strip.text.x = element_blank(),
      panel.spacing.y = unit(7, "lines")
    )
}

theme_bottom <- function(size_text = 38, size_title = 42, size_legend = 32, color = "#344771") {
  base_theme(
    size_text, size_title, size_legend, color,
    grid_major = element_line(linetype = "dashed"),
    legend_position = "bottom"
  ) +
    theme(
      axis.title.x = element_text(size = size_title, face = "bold"),
      axis.text.x = element_text(color = color, size = size_title),
      strip.text.x = element_blank(),
      legend.justification = "right"
      
    )
}

base_plot_comp <- function(data, title, y_labels, y_limits) {
  ggplot(data, aes(condition, measurement, fill = countryregion, label = countryregion)) +
    geom_line(aes(color = countryregion, size = countryregion, linetype = countryregion)) + 
    geom_point(aes(color = countryregion)) +
    scale_size_manual(values = c(1.4, 1.4, 1.4)) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    labs(title = title, y = "", x = "") +
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
    scale_x_continuous(
      breaks = c(1990, 2020, 2050, 2100), 
      labels = c("1990", "2020", "2050", "2100")
    ) +
    scale_color_manual(values = pal) +
    scale_y_continuous(breaks = y_labels, labels = y_labels, limits = y_limits)
}

# Add labels to the plot
enrich_with_labels <- function(plot, label_data, curvature) {
  plot +
    geom_label(
      data = label_data,
      aes(label = name_lab, colour = countryregion), 
      fill = 'white', 
      size = 5,
      check_overlap = FALSE
    ) +
    geom_text_repel(
      aes(color = countryregion, label = lab),
      segment.angle = 1,
      size = 4,
      vjust = -1,
      segment.size = 0.5,
      segment.alpha = 0.5,
      segment.linetype = "dotted",
      segment.curvature = curvature
    )
}

# Add curves to the plot
add_curves <- function(plot) {
  plot +
    geom_curve(aes(x = 1990, y = -5000, xend = 2019, yend = 4000), 
               inherit.aes = FALSE, curvature = 0.2, color = "grey") +
    geom_curve(aes(x = 2021, y = -5000, xend = 2049, yend = 4000), 
               inherit.aes = FALSE, curvature = 0.2, color = "grey") +
    geom_curve(aes(x = 2051, y = -5000, xend = 2100, yend = 4000), 
               inherit.aes = FALSE, curvature = 0.2, color = "grey")
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
      labs(y = "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0Observations\n(Count)", face = 'bold') +
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



indi_coun_regi <- function(
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


country_average <- function(
    data,
    vars,
    labels,
    forma_theme,
    f_labels_data,
    filter_condition = NULL,
    size_count = 10,
    geom_text_y = 0.25
){
  # Create labels for the facets
  condition.labs <- labels
  names(condition.labs) <- vars
  
  # Ensure levels for `age_group5` include an empty string
  levels(data$age_group5) <- c(levels(data$age_group5), '')
  
  # Create the f_labels data frame
  f_labels <- f_labels_data
  f_labels$condition <- factor(f_labels$condition)
  
  # Apply filter condition if provided
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))
  }
  
  # Create the plot
  graph <- data %>%
    mutate(label = ifelse(country == "Average", country, NA)) %>%
    ggplot(
      aes(x = age_group5, 
          y = measurement, 
          colour = I(ifelse(country == "Average", "#050A30", "#D3D3D3")), 
          group = country)
    ) +
    facet_wrap(
      . ~condition, 
      ncol = 3, 
      labeller = labeller(condition = condition.labs)
    ) +
    geom_rect(
      aes(xmin = "60", xmax = "80", ymin = -Inf, ymax = Inf),
      alpha = 0.006,
      fill = "#344771"
    ) +
    theme_minimal() +
    geom_line(size = 1.1) +
    geom_line(
      data = filter(data, (country == "Average")), 
      size = 2, 
      linetype = "solid"
    ) +
    geom_text(
      x = "40", 
      y = geom_text_y, 
      aes(label = label), 
      data = f_labels, 
      size = 9, 
      color = "#050A30"
    ) +
    scale_fill_manual(values = c("#D3D3D3")) +
    forma_theme
  
  return(graph)
}

forma_axes <- function(plot, y_label = "Proportion", x_label = "Age", bre_age, lab_age) {
  plot +
    labs(y = y_label, x = x_label) +
    scale_x_discrete(
      breaks = bre_age,
      labels = lab_age,
      position = "bottom"
    ) +
    scale_y_continuous(
      breaks = c(0, 0.5, 1),
      labels = c('0', '–', '1'),
      limits = c(0,1)
    )
}


# Shared transformation function
transformation <- function(
    data,
    var_vec,
    ids,
    var_nam,
    val_nam,
    summarize_group_vars = NULL,
    country_transform = FALSE
) {
  # Ensure 'age_group5' is a factor and normalize 'work_hrs'
  data$age_group5 <- as.factor(data$age_group5)
  data$work_hrs <- data$work_hrs / 40
  
  # Reshape data using melt
  data <- melt(
    data,
    id.vars = ids,
    measure.vars = var_vec,
    variable.name = var_nam,
    value.name = val_nam
  )
  
  # Apply filtering to exclude certain age groups
  data <- data %>%
    filter(!age_group5 %in% c("0", "5", "10", "15"))
  
  # Optionally transform country codes into full names
  if (country_transform) {
    data <- data %>%
      mutate(
        country = case_when(
          country == "eth" ~ "Ethiopia",
          country == "mlw" ~ "Malawi",
          country == "nga" ~ "Nigeria",
          country == "tza" ~ "Tanzania",
          country == "gha" ~ "Ghana",
          country == "saf" ~ "South~Africa",
          country == "uga" ~ "Uganda",
          country == "niger" ~ "Niger",
          country == "ssa" ~ "Average",
          TRUE ~ NA_character_
        )
      )
  }
  
  # Optionally summarize the data by specific groups
  if (!is.null(summarize_group_vars)) {
    data <- data %>%
      group_by(across(all_of(summarize_group_vars))) %>%
      summarize(
        !!val_nam := mean(get(val_nam), na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(data)
}
# Country-specific transformation
count_avera <- function(
    data,
    var_vec,
    ids = c("country", "age_group5"),
    var_nam = "condition",
    val_nam = "measurement"
) {
  transformation(
    data = data,
    var_vec = var_vec,
    ids = ids,
    var_nam = var_nam,
    val_nam = val_nam,
    summarize_group_vars = NULL,  # No summarization needed for country data
    country_transform = TRUE  # Apply country name transformation
  )
}

# Gender-specific transformation
count_gender <- function(
    data,
    var_vec,
    ids = c("female", "age_group5"),
    var_nam = "condition",
    val_nam = "measurement"
) {
  transformation(
    data = data,
    var_vec = var_vec,
    ids = ids,
    var_nam = var_nam,
    val_nam = val_nam,
    summarize_group_vars = c("female", "age_group5", var_nam),
    country_transform = FALSE  # No country transformation for gender data
  )
}
