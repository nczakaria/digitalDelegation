source("1_dataCleaning.R")
source("2_plotFunctions.R")
source("2_modelFunctions.R")
library(ggplot2)

setwd("~/Desktop/GithubCode")
test.data.sg <- read_csv("test.data.sg.csv",show_col_types = FALSE)
test.data.us <- read_csv("test.data.us.csv",show_col_types = FALSE)


# Financial Digital Delegation
# Fit US financial model
excluded_vars_financial <- c("lowstakeMed", "highstakeMed", "MedicalDelegate1", "MedicalDelegate2", "FinanceDelegate2")
test.data.us$Marital <- as.factor(test.data.us$Marital)
test.data.us$Marital <- relevel(test.data.us$Marital, ref = "Once Married")
modelFinLevel1_new <- fit_logistic_model(test.data.us, "FinanceDelegate1", excluded_vars_financial)

# Medical Digital Delegation
# Fit US medical model
excluded_vars_medical <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel1_new <- fit_logistic_model(test.data.us, "MedicalDelegate1", excluded_vars_medical)


# SG data on US model
## Fit SG data on US financial model
independent_vars_financial <- c("Age", "Income", "Gender", "Education", "Marital", "IADL", "ADL")
test.data.sg$Marital <- as.factor(test.data.sg$Marital)
test.data.sg$Marital <- relevel(test.data.sg$Marital, ref = "Once Married")
modelFinLevel2 <- SG_data_on_US_model(test.data.sg, "FinanceDelegate1", independent_vars_financial)

## Fit SG data on US Medical model
independent_vars_medical <- c("Age", "Gender", "Education", "Employment", "Siblings", "Ethnicity", "Marital", "IADL", "ADL")
modelMedLevel2 <- SG_data_on_US_model(test.data.sg, "MedicalDelegate1", independent_vars_medical)


# SG model
## Fit SG financial model
excluded_vars_financial_SG <- c("lowstakeMed", "highstakeMed", "MedicalDelegate1", "MedicalDelegate2", "FinanceDelegate2")
modelFinLevel3_new <- fit_logistic_model(test.data.sg, "FinanceDelegate1", excluded_vars_financial_SG)

## Fit SG Medical model
excluded_vars_medical_SG <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel3_new <- fit_logistic_model(test.data.sg, "MedicalDelegate1", excluded_vars_medical_SG)

save_model_summary_to_csv(modelFinLevel1_new, "Financial_US_Model.csv")
save_model_summary_to_csv(modelMedLevel1_new, "Medical_US_Model.csv")
save_model_summary_to_csv(modelFinLevel2, "Financial_SG_on_US_Model.csv")
save_model_summary_to_csv(modelMedLevel2, "Medical_SG_on_US_Model.csv")
save_model_summary_to_csv(modelFinLevel3_new, "Financial_SG_Model.csv")
save_model_summary_to_csv(modelMedLevel3_new, "Medical_SG_Model.csv")

