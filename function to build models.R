#########-----------_financialDigitalDelegation START HERE-----------------
#Assist vs. No
# Function for building logistic regression models
fit_logistic_model <- function(data, dependent_var, excluded_vars) {
  # recode the dependent variable
  data[[dependent_var]] <- factor(data[[dependent_var]], levels = c("Assist", "No"), labels = c("1", "0"))
  data[[dependent_var]] <- as.numeric(as.character(data[[dependent_var]]))
  # use ~ to link logistic regression to exclude certain variables (y ~ -x1)
  formula_str <- paste(dependent_var, "~ .", paste("-", excluded_vars, collapse = " "))
  formula <- as.formula(formula_str)
  # fit logistic regression model
  model <- glm(formula, data = data, family = "binomial")
  print(summary(model))
  # apply backward selection to have the lowest AIC value
  model_new <- stepAIC(model)
  print(summary(model_new$fitted.values))
  print(summary(model_new))
  # calculate confidence intervals for the model coefficients
  print(confint(model_new, level = 0.95))
  print(confint.default(model_new))
  return(model_new)
}

# # Function to fit SG data on the US model
SG_data_on_US_model <- function(data, dependent_var, independent_vars) {
  # recode the dependent variable
  data[[dependent_var]] <- factor(data[[dependent_var]], levels = c("Assist", "No"), labels = c("1", "0"))
  data[[dependent_var]] <- as.numeric(as.character(data[[dependent_var]]))
  # use ~ to link logistic regression to include certain variables (y ~ x1 + x2)
  formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  # fit logistic regression model
  model <- glm(formula, data = data, family = "binomial")
  # apply backward selection to have the lowest AIC value
  print(summary(model$fitted.values))
  print(summary(model))
  # calculate confidence intervals for the model coefficients
  print(confint(model, level = 0.95))
  print(confint.default(model))
  return(model)
}
