# Plot function for Formal, Informal, No Digital Proxies in SG
create_proxy_plot <- function(data, title, fill_colors, output_filename) {
  # use ggplot to make barplot
  plot <- ggplot(data, aes(x = Proxy, y = Percent, fill = Proxy)) +
    # draw bar plot with grouping
    geom_bar(stat = "identity", position = "dodge") +
    # add percentage on bars
    geom_text(aes(label = paste0(Percent, "%"), y = Percent + 1), position = position_dodge(width = 1.0), vjust = 0) +
    # add count on bars
    geom_text(aes(label = Number, y = Percent - 6), position = position_dodge(width = 0.9), vjust = 0) +
    # add title, x-aixs, y-axis labels
    labs(title = title,
         x = "Proxy Type",
         y = "Percentage and Count of Digital Proxies in SG") +
    # fill colors
    scale_fill_manual(values = fill_colors) +
    theme_minimal()
  # save the plot into a file
  ggsave(output_filename, plot = plot, width = 10, height = 8, dpi = 300)
}

# Function to plot bar plots with trend lines
plot_barplot_with_trend <- function(data, x_var, y_var, group_var, 
                                    fill_colors, trend_colors, 
                                    x_labels, title, x_lab, y_lab,
                                    levels = NULL,
                                    output_filename) {
  if (!is.null(levels)) {
    data[[x_var]] <- factor(data[[x_var]], levels = levels)
  } else {
    data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))
  }
  levels(data[[x_var]]) <- x_labels
  p <- ggplot(data, aes_string(x = x_var, y = y_var, group = group_var)) +
    geom_bar(aes_string(fill = group_var), stat = 'identity', position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors) +
    geom_smooth(aes_string(color = group_var), method = "lm",formula = y ~ x, se = FALSE, size = 1) +
    geom_text(aes_string(label = "sprintf('%.1f%%', Proportion)"),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 5) +
    scale_color_manual(values = trend_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16)) +
    labs(x = x_lab,
         y = y_lab,
         title = title) +
    guides(fill = guide_legend(title = group_var), color = guide_legend(title = group_var))
  
  ggsave(output_filename, plot = p, width = 10, height = 8, dpi = 300)
}

# Function to plot bar plots without trend lines
plot_barplot_no <- function(data, x_var, y_var, group_var, x_labels, fill_colors,title, x_lab, y_lab, output_filename) {
  # recode variable with specified labels
  data[[x_var]] <- factor(data[[x_var]], levels = names(x_labels), labels = x_labels)
  # draw bar plot with grouping
  p <- ggplot(data, aes_string(x = x_var, y = y_var, group = group_var)) +
    geom_bar(aes_string(fill = group_var), stat = 'identity', position = position_dodge(width = 0.8)) +
    # fill colors
    scale_fill_manual(values = fill_colors) +
    # add text labels on bars
    geom_text(aes(label = Count), position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1, size = 12)) +
    labs(title = title, x = x_lab, y = y_lab) +
    guides(fill = guide_legend(title = group_var), color = guide_legend(title = group_var))
  ggsave(output_filename, plot = p, width = 15, height = 8, dpi = 300)
}

# function for plotting box plots
generate_boxplot <- function(data, value_col, output_filename) {
  data$MedicalDelegate1 = factor(data$MedicalDelegate1,levels = c("Assist","No"),labels = c("1","0"))
  data$FinanceDelegate1 = factor(data$FinanceDelegate1,levels = c("Assist","No"),labels = c("1","0"))
  # make the dataset into long data format
  long_data <- data %>%
    pivot_longer(cols = c(FinanceDelegate1, MedicalDelegate1), 
                 names_to = "Type", 
                 values_to = "Delegate", 
                 values_drop_na = TRUE) %>%
    filter(Delegate == 1) %>%
    select(-Delegate)
  # calculate IQR values for each type
  iqr_values <- long_data %>% group_by(Type) %>%
    summarise(Q1 = quantile(!!sym(value_col), 0.25), Q3 = quantile(!!sym(value_col), 0.75), IQR = IQR(!!sym(value_col))) %>%
    ungroup()
  # function to check whether this value is an outlier (< lower bound or > upper bound)
  is_outlier <- function(value, type) {
    iqr_row <- iqr_values %>% filter(Type == type)
    lower_bound <- iqr_row$Q1 - 1.5 * iqr_row$IQR
    upper_bound <- iqr_row$Q3 + 1.5 * iqr_row$IQR
    value < lower_bound | value > upper_bound
  }
  # mark outliers using mapply
  long_data <- long_data %>%
    mutate(Outlier = mapply(is_outlier, !!sym(value_col), Type))
  # draw barplot
  boxplot <- ggplot(long_data, aes(x = Type, y = !!sym(value_col))) +
    geom_boxplot() +
    geom_text(
      aes(label = ifelse(Outlier, as.character(!!sym(value_col)), "")),
      position = position_dodge(width = 0.25), hjust = -0.3, vjust = 0, size = 2
    ) +
    theme_minimal() +
    xlab("Types of Digital Proxies") +
    ylab(value_col) +
    # ensure text size
    scale_x_discrete(labels = c("FinanceDelegate1" = "Finance", "MedicalDelegate1" = "Medical")) +
    coord_flip()
  ggsave(output_filename, plot = boxplot, width = 10, height = 8, dpi = 300)
}

# function for plotting BEHAVIORS OF DIGITAL PROXIES
plot_delegation_reasons <- function(response_order, cleaned_data_finance, cleaned_data_medical, output_filename, x_lab = NULL, y_lab = NULL) {
  # add delegateType column to the datasets
  cleaned_data_finance <- cleaned_data_finance %>% mutate(delegateType = 'Finance')
  cleaned_data_medical <- cleaned_data_medical %>% mutate(delegateType = 'Medical')
  # combine two dataset
  combined_data <- rbind(cleaned_data_finance, cleaned_data_medical)
  # filter all NAs
  combined_data <- combined_data %>% filter(!is.na(Response) & Response != "NA")
  # calculate counts of each response by delegateType (use groupby function)
  combined_counts <- combined_data %>%
    group_by(Response, delegateType) %>%
    summarise(Count = n(),.groups = 'drop')
  # filter all NAs
  combined_counts <- combined_counts %>% filter(!is.na(Response) & Response != "NA")
  # recode the Response variable
  combined_counts$Response <- factor(combined_counts$Response, levels = response_order)
  # draw barplot
  manage_account <- ggplot(combined_counts, aes(x = Response, y = Count, fill = delegateType)) +
    geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) +
    # add counts on bars
    geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(width = 0.9), size = 2.5) +
    theme_minimal() +
    # ensure plot margins and text
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
    scale_fill_manual(values = c('Finance' = 'darkgreen', 'Medical' = 'darkblue'))
    # add x-axis, y-axis
    if (!is.null(x_lab)) {
      manage_account <- manage_account + labs(x = x_lab)
    }
  if (!is.null(y_lab)) {
    manage_account <- manage_account + labs(y = y_lab)
  }
  ggsave(output_filename, plot = manage_account, width = 10, height = 8, dpi = 300)
}


# function for plotting Distribution among financial digital proxies, normalized across practice type
create_proxy_plot_new <- function(counts, delegate_column, fill_colors, x_label, y_label, legend_title, output_filename) {
  # make barplot with AgeGender on the x-axis and Count on the y-axis
  proxy_plot <- ggplot(counts, aes(x = AgeGender, y = Count, fill = !!sym(delegate_column))) +
    # draw bar plot with grouping
    geom_bar(stat = 'identity', position = position_dodge(width = 0.8)) +
    # fill colors
    scale_fill_manual(values = fill_colors) +
    # text labels on bars
    geom_text(aes(label = paste0("#", Count)),
              position = position_dodge(width = 0.8), vjust = -0.25, size = 3) +
    theme_minimal() +
    # make x-axis, y-axis labels
    labs(x = x_label, y = y_label, fill = legend_title) +
    # ensure legend title
    guides(fill = guide_legend(title = legend_title))
  ggsave(output_filename, plot = proxy_plot, width = 10, height = 8, dpi = 300)
}


