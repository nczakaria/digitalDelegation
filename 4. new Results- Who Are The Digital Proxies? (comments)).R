setwd("~/Desktop/new R script with comments")
source("1. Initial Data Cleaning.R")
source("generic function for ploting.R")

library(ROCR)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)

# SG Results
# Figure 3
## set columns to numeric
Age<-as.numeric(test.data.sg$Age)
Housing<-as.numeric(test.data.sg$Housing)
Income<-as.numeric(test.data.sg$Income)
Gender<-as.numeric(test.data.sg$Gender)
Education<-as.numeric(test.data.sg$Education)
Employment<-as.numeric(test.data.sg$Employment)
Siblings<-as.numeric(test.data.sg$Siblings)
Ethnicity<-as.numeric(test.data.sg$Ethnicity)
Marital<-as.numeric(test.data.sg$Marital)
ADL<-as.numeric(test.data.sg$ADL)
IADL<-as.numeric(test.data.sg$IADL)
# compute the correlation matrix with correlation value
ratings <- data.frame(Age,Housing,Income,Gender,Education,Employment,Siblings,Ethnicity,Marital,ADL,IADL)
correlation_ratings<-cor(ratings)
# plot correlation matrix
corrplot(correlation_ratings, method = "number",tl.col =4)
jpeg("correlation.jpg", width = 10, height = 8, units = "in", res = 300)
corrplot(correlation_ratings, method = "number",tl.col =4)
dev.off()

# new dataframe for US Finance
new_finance_data_us <- test.data.us %>%
  select(FinanceDelegate1, Age, Income, Gender, Education, Marital, IADL, ADL)
# new dataframe for SG Finance
new_finance_data_sg <- test.data.sg %>%
  select(FinanceDelegate1, Age, Income, Gender, Education, Marital, IADL, ADL)

# Training US Financial model (US dataset)
model_us_fin <- glm(FinanceDelegate1 ~ Age + Income + Gender + Education + Marital + IADL + ADL, data=new_finance_data_us, family = "binomial")


# new dataframe for US Medical
new_medical_data_us <- test.data.us %>%
  select(MedicalDelegate1, Age, Gender, Education, Employment, Siblings, Ethnicity, Marital, IADL, ADL)

# new dataframe for SG Medical
new_medical_data_sg <- test.data.sg %>%
  select(MedicalDelegate1, Age, Gender, Education, Employment, Siblings, Ethnicity, Marital, IADL, ADL)

# Training SG Medical model (US dataset)
model_us_med <- glm(MedicalDelegate1 ~ Age + Gender + Education + Employment + Siblings + Ethnicity + Marital + IADL + ADL,family=binomial, data=new_medical_data_us)

# Figure 2 (prevalence of digital helping in the population, categorized by reported relationship to the person who is being helped.)
# select necessary columns from sg_filtered_data
test.data.sg.new = sg_filtered_data[,c('Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital','HelpImmediateFam','HelpRelatives','HelpFriends','FinanceHelpImmediateFam','FinanceHelpRelatives','FinanceHelpFriends','MedicalHelpImmediateFam','MedicalHelpRelatives','MedicalHelpFriends','MedicalDelegate1','MedicalDelegate2','FinanceDelegate1','FinanceDelegate2')]
# filter data to assit both financial and medical, only assist financial, only assist medical
filtered_data_sg_new <- test.data.sg.new %>%
  filter(
    (MedicalDelegate1 == "Assist" & FinanceDelegate1 == "Assist") |  # Both
      (MedicalDelegate1 == "No" & FinanceDelegate1 == "Assist") |      # Only finance
      (MedicalDelegate1 == "Assist" & FinanceDelegate1 == "No")        # Only medical
  )
# Calculate the total number of respondents for each help category
total_respondents <- test.data.sg.new %>%
  summarise(
    Family = sum(HelpImmediateFam > 0),
    Relatives = sum(HelpRelatives > 0),
    Friends = sum(HelpFriends > 0)
  ) %>%
  mutate(Category = "Total Respondents")

# Calculate the total number of digital proxies for each help category
total_proxies <- filtered_data_sg_new %>%
  summarise(
    Family = sum(HelpImmediateFam > 0),
    Relatives = sum(HelpRelatives > 0),
    Friends = sum(HelpFriends > 0)
  ) %>%
  mutate(Category = "Digital Proxies")

# Combine the two summaries into one data frame for plotting
total_data <- bind_rows(total_proxies,total_respondents) %>%
  pivot_longer(cols = -Category, names_to = "Relationship", values_to = "Count")
total_data$Category <- factor(total_data$Category, levels = c("Total Respondents", "Digital Proxies"))

# make the bar plot for prevalence of digital helping in the population, categorized by reported relationship to the person who is being helped.
plot_barplot_no(
  data = total_data,
  x_var = "Relationship",
  y_var = "Count",
  group_var = "Category",
  x_labels = c(Family = "Family", Relatives = "Relatives", Friends = "Friends"),
  fill_colors = c("Total Respondents" = "grey30", "Digital Proxies" = "grey80"),
  title = "",
  x_lab = "Relationship type of assisted adults",
  y_lab = "Number of Respondents",
  output_filename = "relationship.jpg"
)


# Figure 4 (digital proxies by age and gender reveal younger adults of the male gender)
# filter data to assit both financial and medical, only assist financial, only assist medical
filtered_data_sg <- test.data.sg %>%
  filter(
    (MedicalDelegate1 == 1 & FinanceDelegate1 == 1) |  # Both
      (MedicalDelegate1 == 0 & FinanceDelegate1 == 1) |      # Only finance
      (MedicalDelegate1 == 1 & FinanceDelegate1 == 0)        # Only medical
  )
# Calculate the total number of individuals by gender
gender_female <- filtered_data_sg %>%
  filter(Gender == 1)
gender_male <- filtered_data_sg %>%
  filter(Gender == 2)
# levels = c(1, 2), labels = c("Female","Male")
# aggregate counts and calculate proportions for females
count <- aggregate(Gender ~ Age, data = gender_female, length)
sum <- sum (table(filtered_data_sg$Age))
count$Proportion <- count$Gender / sum * 100
# aggregate counts and calculate proportions for males
count_not_proxies <- aggregate(Gender ~ Age, data = gender_male, length)
sum_not <- sum (table(filtered_data_sg$Age))
count_not_proxies$Proportion <- count_not_proxies$Gender / sum_not * 100

count$Type <- 'Female'
count_not_proxies$Type <- 'Male'
# recode Age and Gender
count$Age <- factor(count$Age,levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                    labels = c("21-24","25-29", "30-24", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", ">=70"))
count_not_proxies$Age <- factor(count_not_proxies$Age,levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                labels = c("21-24","25-29", "30-24", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", ">=70"))
count$Gender <- factor(count$Gender,levels = c(1, 2),
                       labels = c("Female","Male"))
count_not_proxies$Gender <- factor(count_not_proxies$Gender,levels = c(1, 2),
                                   labels = c("Female","Male"))
# conbine two datasets
combined_data <- rbind(count,count_not_proxies)
# draw barplot about digital proxies by age and gender reveal younger adults of the male gender
plot_barplot_with_trend(
  data = combined_data,
  x_var = "Age",
  y_var = "Proportion",
  group_var = "Type",
  fill_colors = c("Female" = "darkgrey", "Male" = "lightgrey"),
  trend_colors = c("Female" = "red", "Male" = "blue"),
  x_labels = c("21-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", ">=70"),
  title = "Digital Delegation by Age",
  x_lab = "Age",
  y_lab = "Percentage of Unique Digital Delegation",
  output_filename = "Age&Gender.jpg"
)


# proportion function to calculate proportions of proxies and non-proxies for a given variable
calculate_proportions <- function(data, variable, proxy_var) {
  # filter data for proxies (1) and non-proxies (0)
  proxies <- data %>% filter(!!as.name(proxy_var) == 1)
  non_proxies <- data %>% filter(!!as.name(proxy_var) == 0)
  # count the number of proxies for each level of the variable
  count_proxies <- aggregate(as.formula(paste(proxy_var, "~", variable)), data = proxies, length)
  total_proxies <- sum(table(proxies[[variable]]))
  count_proxies$Proportion <- count_proxies[[proxy_var]] / total_proxies * 100
  # count the number of non-proxies for each level of the variable
  count_non_proxies <- aggregate(as.formula(paste(proxy_var, "~", variable)), data = non_proxies, length)
  total_non_proxies <- sum(table(non_proxies[[variable]]))
  count_non_proxies$Proportion <- count_non_proxies[[proxy_var]] / total_non_proxies * 100
  
  count_proxies$Type <- 'Proxies'
  count_non_proxies$Type <- 'Non-Proxies'
  # combine two dataset
  combined_data <- rbind(count_proxies, count_non_proxies)
  combined_data[[variable]] <- as.factor(combined_data[[variable]])
  
  return(combined_data)
}

# Education (Financial) Figure 5
## calculate proportions for Education and FinanceDelegate1 using calculate_proportions function
education_data <- calculate_proportions(test.data.sg, "Education", "FinanceDelegate1")
education_data$Education <- factor(education_data$Education, levels = c(1, 2, 3),
                                   labels = c("< High School", "High School", "Degree and above"))
# draw barplot for financial delegation by education
plot_barplot_with_trend(
  data = education_data,
  x_var = "Education",
  y_var = "Proportion",
  group_var = "Type",
  fill_colors = c("Proxies" = "lightgreen", "Non-Proxies" = "darkgreen"),
  trend_colors = c("Proxies" = "lightgreen", "Non-Proxies" = "darkgreen"),
  x_labels = c("High School", "Degree and above", "<High School" ),
  title = "Financial Delegation by Education",
  x_lab = "Education",
  y_lab = "Percentage of Financial Delegation",
  output_filename = "Education.jpg"
)


# Income (Financial) Figure 6
## calculate proportions for Income and FinanceDelegate1 using calculate_proportions function
income_data <- calculate_proportions(test.data.sg, "Income", "FinanceDelegate1")
income_data$Income <- factor(income_data$Income, levels = c("1", "2", "3", "4", "5", "6"),
                             labels = c("<30k", "30-39k", "50-99k", "100-149k", "150-199k", ">=200k"))
# draw barplot for financial delegation by income
plot_barplot(
  data = income_data,
  x_var = "Income",
  y_var = "Proportion",
  group_var = "Type",
  x_labels = c("<30k" = "<30k", "30-39k" = "30-39k", "50-99k" = "50-99k", "100-149k" = "100-149k", "150-199k" = "150-199k", ">=200k" = ">=200k"),
  fill_colors = c("Proxies" = "lightgreen", "Non-Proxies" = "darkgreen"),
  trend_colors = c("Proxies" = "lightgreen", "Non-Proxies" = "darkgreen"),
  title = "Financial Delegation by Income",
  x_lab = "Income",
  y_lab = "Percentage of Financial Delegation",
  output_filename = "income.jpg"
)


## Siblings (Medical) Figure 7
## calculate proportions for Income and MedicalDelegate1 using calculate_proportions function
siblings_data <- calculate_proportions(test.data.sg, "Siblings", "MedicalDelegate1")
siblings_data$Siblings <- factor(siblings_data$Siblings, levels = c("1", "2", "3","4", "5"),
                                 labels = c("0", "1", "2", "3", ">4"))
# draw barplot for medical delegation by siblings
plot_barplot(
  data = siblings_data,
  x_var = "Siblings",
  y_var = "Proportion",
  group_var = "Type",
  x_labels = c("0" = "0", "1" = "1", "2" = "2", "3" = "3", ">4" = ">4"),
  fill_colors = c("Proxies" = "lightblue", "Non-Proxies" = "darkblue"),
  trend_colors = c("Proxies" = "lightblue", "Non-Proxies" = "darkblue"),
  title = "Medical Delegation by Siblings",
  x_lab = "Siblings",
  y_lab = "Percentage of Medical Delegation",
  output_filename = "Siblings.jpg"
)


## Ethnicity (Medical) Figure 8 (SG ethnicity categorical)
## calculate proportions for Ethnicity and MedicalDelegate1 using calculate_proportions function
ethnicity_data <- calculate_proportions(test.data.sg, "Ethnicity", "MedicalDelegate1")
ethnicity_data$Ethnicity <- factor(ethnicity_data$Ethnicity, levels = c("3", "4", "7", "6", "8", "10"),
                                   labels = c("Chinese", "Eurasian", "Malay", "Indian", "Mixed", "Others"))
# draw barplot for medical delegation by Ethnicity
plot_barplot(
  data = ethnicity_data,
  x_var = "Ethnicity",
  y_var = "Proportion",
  group_var = "Type",
  x_labels = c("Chinese" = "Chinese", "Eurasian" = "Eurasian", "Malay" = "Malay", "Indian" = "Indian", "Mixed" = "Mixed", "Others" = "Others"),
  fill_colors = c("Proxies" = "lightblue", "Non-Proxies" = "darkblue"),
  trend_colors = c("Proxies" = "lightblue", "Non-Proxies" = "darkblue"),
  title = "Medical Delegation by Ethnicity",
  x_lab = "Ethnicity",
  y_lab = "Percentage of Medical Delegation",
  output_filename = "Ethnicity.jpg"
)

# Figure 9
## Boxplot for IADL
generate_boxplot(test.data.sg, "IADL", "boxplot_IADL.jpg")

## Boxplot for ADL
generate_boxplot(test.data.sg, "ADL", "boxplot_ADL.jpg")


path_to_r_script <- "~/Desktop/new R script with comments/who.are.digital.proxies.csv"
write.csv(who.are.digital.proxies, file = path_to_r_script, row.names = FALSE)
