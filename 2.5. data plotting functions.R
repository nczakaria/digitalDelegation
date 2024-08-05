source("2.5. data plotting functions.R")
# plot function for Formal, Informal, No Digital Proxies in SG
create_proxy_plot <- function(data, title, fill_colors, output_filename) {
  plot <- ggplot(data, aes(x = Proxy, y = Percent, fill = Proxy)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%"), y = Percent + 1), position = position_dodge(width = 1.0), vjust = 0) +
    geom_text(aes(label = Number, y = Percent - 6), position = position_dodge(width = 0.9), vjust = 0) +
    labs(title = title,
         x = "Proxy Type",
         y = "Percentage and Count of Digital Proxies in SG") +
    scale_fill_manual(values = fill_colors) +
    theme_minimal()
  ggsave(output_filename, plot = plot, width = 10, height = 8, dpi = 300)
}

# function for plotting ROC and AUC
plot_roc_and_auc <- function(model, new_data, response_var, plot_title, output_filename) {
  predicted_probabilities <- predict(model, new_data, type = "response")
  prediction_obj <- prediction(predicted_probabilities, new_data[[response_var]])
  performance_obj <- performance(prediction_obj, "tpr", "fpr")
  
  jpeg(output_filename, width = 10, height = 8, units = "in", res = 300)
  plot(performance_obj, main = plot_title, col = "blue", lwd = 2,
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  auc_obj <- performance(prediction_obj, "auc")
  auc_value <- unlist(slot(auc_obj, "y.values"))
  cat("AUC:", auc_value, "\n")
  text(0.6, 0.4, paste("AUC =", round(auc_value, 3)), col = "red", cex = 1.2)
  dev.off()
}

# function for plotting bar plots
plot_barplot <- function(data, x_var, y_var, group_var, x_labels, fill_colors, trend_colors, title, x_lab, y_lab, output_filename) {
  data[[x_var]] <- factor(data[[x_var]], levels = names(x_labels), labels = x_labels)
  p <- ggplot(data, aes_string(x = x_var, y = y_var, group = group_var)) +
    geom_bar(aes_string(fill = group_var), stat = 'identity', position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors) +
    geom_smooth(aes_string(color = group_var), method = "lm", se = FALSE, linetype = "dashed", size = 1) +
    geom_text(aes_string(label = paste0(sprintf("%.1f%%", get(y_var))), fill = group_var),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    scale_color_manual(values = trend_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1, size = 12)) +
    labs(title = title, x = x_lab, y = y_lab) +
    guides(fill = guide_legend(title = group_var), color = guide_legend(title = group_var))
  
  ggsave(output_filename, plot = p, width = 10, height = 8, dpi = 300)
}

# function for plotting box plots
generate_boxplot <- function(data, value_col, output_filename) {
  long_data <- data %>%
    pivot_longer(cols = c(FinanceDelegate1, MedicalDelegate1), 
                 names_to = "Type", 
                 values_to = "Delegate", 
                 values_drop_na = TRUE) %>%
    filter(Delegate == 1) %>%
    select(-Delegate)
  
  iqr_values <- long_data %>% group_by(Type) %>%
    summarise(Q1 = quantile(!!sym(value_col), 0.25), Q3 = quantile(!!sym(value_col), 0.75), IQR = IQR(!!sym(value_col))) %>%
    ungroup()
  
  is_outlier <- function(value, type) {
    iqr_row <- iqr_values %>% filter(Type == type)
    lower_bound <- iqr_row$Q1 - 1.5 * iqr_row$IQR
    upper_bound <- iqr_row$Q3 + 1.5 * iqr_row$IQR
    value < lower_bound | value > upper_bound
  }
  
  long_data <- long_data %>%
    mutate(Outlier = mapply(is_outlier, !!sym(value_col), Type))
  
  boxplot <- ggplot(long_data, aes(x = Type, y = !!sym(value_col))) +
    geom_boxplot() +
    geom_text(
      aes(label = ifelse(Outlier, as.character(!!sym(value_col)), "")),
      position = position_dodge(width = 0.25), hjust = -0.3, vjust = 0, size = 2
    ) +
    theme_minimal() +
    xlab("Types of Digital Proxies") +
    ylab(value_col) +
    scale_x_discrete(labels = c("FinanceDelegate1" = "Finance", "MedicalDelegate1" = "Medical")) +
    coord_flip()
  ggsave(output_filename, plot = boxplot, width = 10, height = 8, dpi = 300)
}


# function for plotting BEHAVIORS OF DIGITAL PROXIES
plot_delegation_reasons <- function(response_order, cleaned_data1, cleaned_data4, output_filename) {
  cleaned_data1$delegateType <- 'Finance'
  cleaned_data4$delegateType <- 'Medical'
  combined_data <- rbind(cleaned_data1, cleaned_data4)
  combined_data <- combined_data %>% filter(!is.na(Response) & Response != "NA")
  
  combined_counts <- combined_data %>%
    group_by(Response, delegateType) %>%
    summarise(Count = n()) %>%
    ungroup()
  combined_counts <- combined_counts %>% filter(!is.na(Response) & Response != "NA")
  combined_counts$Response <- factor(combined_counts$Response, levels = response_order)
  
  manage_account <- ggplot(combined_counts, aes(x = Response, y = Count, fill = delegateType)) +
    geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) +
    geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(width = 0.9), size = 2.5) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
    labs(x = 'Reasons for Assisting Delegators with Digital Services', y = 'Counts of Digital Proxies') +
    scale_fill_manual(values = c('Finance' = 'darkgreen', 'Medical' = 'darkblue'))
  ggsave(output_filename, plot = manage_account, width = 10, height = 8, dpi = 300)
}


# function for plotting Distribution among financial digital proxies, normalized across practice type
create_proxy_plot_new <- function(counts, delegate_column, fill_colors, x_label, y_label, legend_title, output_filename) {
  proxy_plot <- ggplot(counts, aes(x = AgeGender, y = Count, fill = !!sym(delegate_column))) +
    geom_bar(stat = 'identity', position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = fill_colors) +
    geom_text(aes(label = paste0("#", Count)),
              position = position_dodge(width = 0.8), vjust = -0.25, size = 3) +
    theme_minimal() +
    labs(x = x_label, y = y_label, fill = legend_title) +
    guides(fill = guide_legend(title = legend_title))
  
  ggsave(output_filename, plot = proxy_plot, width = 10, height = 8, dpi = 300)
}

