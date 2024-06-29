setwd("~/Desktop/R script")
source("2. data_processing_functions.R")

dat <- read_csv("my.dat.csv")
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

us_filtered_data <- country_data(filtered.dat, "SG")
test.data.us <- process_data(us_filtered_data)
sg_filtered_data <- country_data(filtered.dat, "SG")
test.data.sg <- process_data(sg_filtered_data)


# Financial Digital Delegation
excluded_vars_financial <- c("lowstakeMed", "highstakeMed", "MedicalDelegate1", "MedicalDelegate2", "FinanceDelegate2")
modelFinLevel1_new <- fit_logistic_model(test.data.us, "FinanceDelegate1", excluded_vars_financial)


#########-----------_medicalDigitalDelegation START HERE-----------------
#Assist vs. No
excluded_vars_medical <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel1_new <- fit_logistic_model(test.data.us, "MedicalDelegate1", excluded_vars_medical)


# SG data on US model
## SG data on US financial model
independent_vars_financial <- c("Age", "Income", "Gender", "Education", "Marital", "IADL", "ADL")
modelFinLevel2 <- SG_data_on_US_model(test.data.sg, "FinanceDelegate1", independent_vars_financial)

## SG data on US Medical model
independent_vars_medical <- c("Age", "Gender", "Education", "Employment", "Siblings", "Ethnicity", "Marital", "IADL", "ADL")
modelMedLevel2 <- SG_data_on_US_model(test.data.sg, "MedicalDelegate1", independent_vars_medical)


# SG model
## SG financial model
modelFinLevel3_new <- fit_logistic_model(test.data.sg, "FinanceDelegate1", excluded_vars_financial)

# SG Medical model
excluded_vars_medical_SG <- c("lowstakeMed", "highstakeMed", "FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate2")
modelMedLevel3_new <- fit_logistic_model(test.data.sg, "MedicalDelegate1", excluded_vars_medical_SG)

library(ggplot2)
# Formal, Informal, No Digital Proxies in SG
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

# Financial Digital Proxies in SG
data_financial <- data.frame(
  Proxy = c("No", "Informal", "Formal"),
  Number = c(176, 118, 10),
  Percent = c(57.9, 38.8, 3.3)
)
create_proxy_plot(data = data_financial, title = "Financial Digital Proxies in Singapore", fill_colors = c("No" = "gray", "Informal" = "lightgreen", "Formal" = "darkgreen"), output_filename = "informal_financial.jpg"
)

# Medical Digital Proxies in SG
data_medical <- data.frame(
  Proxy = c("No", "Informal", "Formal"),
  Number = c(178, 47, 79),
  Percent = c(58.5, 15.5, 26.0)
)
# Create and save medical proxy plot
create_proxy_plot(data = data_medical, title = "Medical Digital Proxies in Singapore", fill_colors = c("No" = "gray", "Informal" = "lightblue", "Formal" = "darkblue"), output_filename = "informal_medical.jpg"
)





