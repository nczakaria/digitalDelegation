setwd("~/Desktop/new R script with comments")
source("1. Initial Data Cleaning.R")
source("generic function for ploting.R")
source("function to build models.R")
library(ggplot2)
dat <- read_csv("~/Desktop/R script/my.dat.csv")
my.dat <- dat

#CE + CG + CJ
#Y + Y + Y = formal
#Y + Y + N = formal
#N + Y + Y = Formal

#DO + DQ + DT 
#Y + Y + Y = formal
#Y + Y + N = formal
#N + Y + Y = formal

# DATA for Hypothesis Testing-------------------------------------------------
# 1) There is a positive relationship between physical care behaviors (IADLs and ADLs) and digital financial delegation helping behaviors
## filter: only approved survey entries
filtered.dat <- my.dat

# get US dataset
us_filtered_data <- country_data(filtered.dat, "SG")
test.data.us <- process_data(us_filtered_data)
# get SG dataset
sg_filtered_data <- country_data(filtered.dat, "US")
test.data.sg <- process_data(sg_filtered_data)


# Financial Digital Delegation
# Fit US financial model
excluded_vars_financial <- c("lowstakeMed", "highstakeMed", "MedicalDelegate1", "MedicalDelegate2", "FinanceDelegate2")
modelFinLevel1_new <- fit_logistic_model(test.data.us, "FinanceDelegate1", excluded_vars_financial)

# Medical Digital Delegation
# Fit US medical model
excluded_vars_medical <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel1_new <- fit_logistic_model(test.data.us, "MedicalDelegate1", excluded_vars_medical)


# SG data on US model
## Fit SG data on US financial model
independent_vars_financial <- c("Age", "Income", "Gender", "Education", "Marital", "IADL", "ADL")
modelFinLevel2 <- SG_data_on_US_model(test.data.sg, "FinanceDelegate1", independent_vars_financial)

## Fit SG data on US Medical model
independent_vars_medical <- c("Age", "Gender", "Education", "Employment", "Siblings", "Ethnicity", "Marital", "IADL", "ADL")
modelMedLevel2 <- SG_data_on_US_model(test.data.sg, "MedicalDelegate1", independent_vars_medical)


# SG model
## Fit SG financial model
modelFinLevel3_new <- fit_logistic_model(test.data.sg, "FinanceDelegate1", excluded_vars_financial)

## Fit SG Medical model
excluded_vars_medical_SG <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel3_new <- fit_logistic_model(test.data.sg, "MedicalDelegate1", excluded_vars_medical_SG)

# Financial Digital Proxies in SG
## create a data frame with the counts and percentages of financial formal or informal digital proxies in Singapore
data_financial <- data.frame(
  Proxy = c("No", "Informal", "Formal"),
  Number = c(176, 118, 10),
  Percent = c(57.9, 38.8, 3.3)
)
# create and save financial proxy plot using the create_proxy_plot function
create_proxy_plot(data = data_financial, title = "Financial Digital Proxies in Singapore", fill_colors = c("No" = "gray", "Informal" = "lightgreen", "Formal" = "darkgreen"), output_filename = "informal_financial.jpg"
)

# Medical Digital Proxies in SG
## create a data frame with the counts and percentages of medical formal or informal digital proxies in Singapore
data_medical <- data.frame(
  Proxy = c("No", "Informal", "Formal"),
  Number = c(178, 47, 79),
  Percent = c(58.5, 15.5, 26.0)
)
# create and save medical proxy plot using the create_proxy_plot function
create_proxy_plot(data = data_medical, title = "Medical Digital Proxies in Singapore", fill_colors = c("No" = "gray", "Informal" = "lightblue", "Formal" = "darkblue"), output_filename = "informal_medical.jpg"
)





