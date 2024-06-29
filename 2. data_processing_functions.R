# Function to filter out US or SG
country_data <- function(data, country) {
  temp_data <- subset(data, !(Country == country))
  return(temp_data)
}


process_data <- function(filtered_data) {
  filtered_data <- subset(filtered_data, !(HelpFriends == "101"))
  filtered_data <- subset(filtered_data, !(FinanceHelpFriends == "101"))
  filtered_data <- subset(filtered_data, !(MedicalHelpImmediateFam == "101"))
  filtered_data <- subset(filtered_data, !(MedicalHelpFriends == "101"))
  filtered_data <- subset(filtered_data, !(HelpFriends == "31"))
  filtered_data <- subset(filtered_data, !(FinanceHelpFriends == "31"))
  filtered_data <- subset(filtered_data, !(MedicalHelpImmediateFam == "31"))
  filtered_data <- subset(filtered_data, !(MedicalHelpFriends == "31"))
  filtered_data <- subset(filtered_data, !(FinanceJointAccount == "Exclude"))
  filtered_data <- subset(filtered_data, !(Marital == "3"))
  filtered_data$Marital <- factor(filtered_data$Marital, levels = c("Once Married","Unmarried", "Married"))
  # Create test dataframe with only demographic predictors
  test_data <- filtered_data[, c('Age', 'Income', 'Housing', 'Gender', 'Education', 'Employment', 'Ethnicity', 'Siblings', 'Marital')]
  
  #how much help participants have provided for an adult in carrying out their instrumental activities of daily living
  test_data$IADL <- rowSums(filtered_data[, c("HelpTransport", "HelpMed", "HelpFinance", "HelpFood", "HelpLaundry", "HelpHousekeep")])
  test_data$IADL <- scale_values(test_data$IADL)
  test_data$IADL <- round(test_data$IADL, digits = 2)
  
  #how much help participants have provided for an adult in carrying out their activities of daily living
  test_data$ADL <- rowSums(filtered_data[, c("ADLBath", "ADLFeed", "ADLGroom", "ADLIncontinence", "ADLToilet", "ADLTransfer")])
  test_data$ADL <- scale_values(test_data$ADL)
  test_data$ADL <- round(test_data$ADL, digits = 2)
  
  #how much help participants have provided for an adult in carrying out low-stake medical care activities
  test_data$lowstakeMed <- rowSums(filtered_data[, c("MedicalMakeAppt", "MedicalAccompanyAppt", "MedicalTakeMeds", "MedicalSeeTreatment")])
  
  #how much help participants have provided for an adult in carrying out high-stake medical care activities
  test_data$highstakeMed <- rowSums(filtered_data[, c("MedicalDecisionPerson", "MedicalDecisionDoctor", "MedicalSignPapers", "MedicalWorthPerson","MedicalWorthDoctor", "MedicalWorthFamily", "MedicalEndPerson", "MedicalEndFamily","MedicalFlexibilityPerson", "MedicalFlexibilityDoctor", "MedicalFlexibilityFamily","MedicalDoctorQuestions")])
  
  #DEPENDENT VARIABLES HERE
  test_data$FinanceDelegate1 <- as.factor(filtered_data$FinanceDelegate1)
  test_data$FinanceDelegate2 <- as.factor(filtered_data$FinanceDelegate2)
  test_data$MedicalDelegate1 <- as.factor(filtered_data$MedicalDelegate1)
  test_data$MedicalDelegate2 <- as.factor(filtered_data$MedicalDelegate2)
  
  return(test_data)
}

#########-----------_financialDigitalDelegation START HERE-----------------
#Assist vs. No
fit_logistic_model <- function(data, dependent_var, excluded_vars) {
  data[[dependent_var]] <- factor(data[[dependent_var]], levels = c("Assist", "No"), labels = c("1", "0"))
  data[[dependent_var]] <- as.numeric(as.character(data[[dependent_var]]))
  formula_str <- paste(dependent_var, "~ .", paste("-", excluded_vars, collapse = " "))
  formula <- as.formula(formula_str)
  
  model <- glm(formula, data = data, family = "binomial")
  print(summary(model))
  
  model_new <- stepAIC(model)
  print(summary(model_new$fitted.values))
  print(summary(model_new))
  
  print(confint(model_new, level = 0.95))
  print(confint.default(model_new))
  return(model_new)
}

# SG data on US model
SG_data_on_US_model <- function(data, dependent_var, independent_vars) {
  data[[dependent_var]] <- factor(data[[dependent_var]], levels = c("Assist", "No"), labels = c("1", "0"))
  data[[dependent_var]] <- as.numeric(as.character(data[[dependent_var]]))
  formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  
  model <- glm(formula, data = data, family = "binomial")
  
  print(summary(model$fitted.values))
  print(summary(model))
  
  print(confint(model, level = 0.95))
  print(confint.default(model))
  
  return(model)
}

# bar plot functions
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

