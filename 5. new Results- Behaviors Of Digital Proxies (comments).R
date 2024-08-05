setwd("~/Desktop/new R script with comments")
source("1. Initial Data Cleaning.R")
source("generic function for ploting.R")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
test.data.sg <- sg_filtered_data[, c('Country', 'Age', 'Income', 'Housing', 'Gender', 'Education', 'Employment', 'Ethnicity', 'Siblings', 'Marital', 'FinanceManageReasons', 'FinanceManageReasonsOthers', 'FinanceLoginWays', 'FinanceServiceWays', 'DigitalAssistReasons', 'DigitalAssistReasonsOthers', 'DigitalAssistAccessWays','DigitalAssistUseWays', 'FinanceDelegate1', 'FinanceDelegate2', 'MedicalDelegate1', 'MedicalDelegate2')]

# replace periods (.) in DigitalAssistReasons with an empty string when followed by | or " (because in the survey, this variable has different version for finance and medical)
test.data.sg <- test.data.sg %>%
  mutate(DigitalAssistReasons = str_replace_all(DigitalAssistReasons, "\\.(?=\\||\")", "")) %>%
  mutate(DigitalAssistReasons = str_replace_all(DigitalAssistReasons, "\\.($|\\s)", ""))

# recode reasons for specified columns
recode_reasons <- function(data, column_name, new_column_name) {
  data <- data %>%
    mutate(!!new_column_name := recode(!!sym(column_name),
                                       "The app was not user-friendly for them" = "notUserFriendly",
                                       "They see using the app as a difficult task that should be avoided" = "taskDifficulty",
                                       "They have concerns about sharing their data with third parties" = "thirdPartyConcerns",
                                       "They are unfamiliar with using a technology device to access the app" = "lackAppFamiliarity",
                                       "They have physical limitations (e.g., poor vision and poor motor skills) that constrain their ability to use the app" = "physicalLimitation",
                                       "They do not have a device or equipment to access their digital accounts" = "noDeviceAccess",
                                       "They have cognitive limitations (e.g., existing conditions and cognitive decline with age) that constrain their ability to use the app" = "cognitiveLimiation",
                                       "They have physical limitations (e.g., poor vision and poor motor skills) that constrain their ability to use the app,Others (please specify)" = "physicalLimitation",
                                       "Others (please specify)" = "others",
                                       "Others (Please Specify)" = "others",
                                       .default = !!sym(column_name)))
  return(data)}

# recode FinanceManageReasons and create a new column
raw_digital_proxy_1 <- recode_reasons(test.data.sg, "FinanceManageReasons", "FinanceManageReasons_new")
# create a survey data frame
survey_data_1 <- data.frame(ID = 1:304, Response = raw_digital_proxy_1$FinanceManageReasons_new)
# function to clean and reshape the survey data
clean_data <- function(data, response_column, output_type = "text") {
  data <- data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      not_user_friendly = ifelse(str_detect(!!sym(response_column), "notUserFriendly"), ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      difficult = ifelse(str_detect(!!sym(response_column), "taskDifficulty"), ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      concerns_data = ifelse(str_detect(!!sym(response_column), "thirdPartyConcerns"), ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      unfamiliar = ifelse(str_detect(!!sym(response_column), "lackAppFamiliarity"), ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      physical_limitations = ifelse(!!sym(response_column) == "physicalLimitation",  ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      cognitive_limiation = ifelse(!!sym(response_column) == "cognitiveLimiation", ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      no_device = ifelse(!!sym(response_column) == "noDeviceAccess", ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0)),
      Others = ifelse(!!sym(response_column) == "others", ifelse(output_type == "text", "Yes", 1), ifelse(output_type == "text", "No", 0))) %>%
    group_by(ID)
  return(data)}

# clean and reshape the survey data for DigitalAssistReasons
cleaned_data1 <- clean_data(survey_data_1, "Response")
cleaned_data1 <- recode_reasons(cleaned_data1, "Response", "Response")
cleaned_data1 <- clean_data(cleaned_data1, "Response", output_type = "numeric")
cleaned_data1 <- na.omit(cleaned_data1)

raw_digital_proxy_4 <- recode_reasons(test.data.sg, "DigitalAssistReasons", "DigitalAssistReasons_new")
survey_data4 <- data.frame(ID = 1:304, Response = raw_digital_proxy_4$DigitalAssistReasons_new)
cleaned_data4 <- clean_data(survey_data4, "Response")
cleaned_data4 <- recode_reasons(cleaned_data4, "Response", "Response")
cleaned_data4 <- clean_data(cleaned_data4, "Response", output_type = "numeric")
cleaned_data4 <- na.omit(cleaned_data4)

# assign delegateType and combine two datasets
cleaned_data1$delegateType <- 'Finance'
cleaned_data4$delegateType <- 'Medical'
combined_data <- rbind(cleaned_data1, cleaned_data4)
combined_data <- combined_data %>% filter(!is.na(Response) & Response != "NA")

# calculate counts and create the plot
combined_counts <- combined_data %>%
  group_by(Response, delegateType) %>%
  summarise(Count = n(), .groups = 'drop')
response_order <- c("lackAppFamiliarity", "taskDifficulty", "notUserFriendly", "physicalLimitation", "cognitiveLimiation", "thirdPartyConcerns", "noDeviceAccess", "others")
combined_counts$Response <- factor(combined_counts$Response, levels = response_order)

fill_colors <- c('Finance' = 'darkgreen', 'Medical' = 'darkblue')
plot_barplot_no(
  data = combined_counts,
  x_var = "Response",
  y_var = "Count",
  group_var = "delegateType",
  x_labels = c("lackAppFamiliarity" = "Lack of Familiarity", "taskDifficulty" = "Task Difficulty", "notUserFriendly" = "Not User Friendly","physicalLimitation" = "Physical Limitation","cognitiveLimiation" = "Cognitive Limitation", "thirdPartyConcerns" = "Third Party Concerns","noDeviceAccess" = "No Device Access","others" = "Others"),
  fill_colors = fill_colors,
  title = "Reasons for Assisting Delegators with Digital Services",
  x_lab = "Reasons for Assisting Delegators with Digital Services",
  y_lab = "Counts of Digital Proxies",
  output_filename = "manage_account.jpg"
)

# Figure 11
## Figure 11 in count (SG)
# select necessary columns from sg_filtered_data with only demographic predictors
test.data.sg = sg_filtered_data[,c('Country','Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital','FinanceManageReasons','FinanceManageReasonsOthers','FinanceLoginWays','FinanceServiceWays','DigitalAssistReasons','DigitalAssistReasonsOthers','DigitalAssistAccessWays','DigitalAssistUseWays','FinanceDelegate1','FinanceDelegate2','MedicalDelegate1','MedicalDelegate2','FinanceJointAccount')]
# recode Age and Gender
test.data.sg$Age_new <- factor(test.data.sg$Age,levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                               labels = c("21","25", "30", "35", "40", "45", "50", "55", "60", "65", "70"))
test.data.sg$Age_new <- ifelse(test.data.sg$Age_new %in% c("60","65", "70"), "Older", "Younger")
test.data.sg$Gender_new <- factor(test.data.sg$Gender,levels = c(1, 2), labels = c("Female","Male"))
test.data.sg$AgeGender <- with(test.data.sg, paste(Age_new, Gender_new, sep=":"))

# function to calculate counts and proportions for a specific delegate column
calculate_counts_and_proportions <- function(data, delegate_column, delegate_values, age_gender_levels, delegate_labels) {
  filtered_data <- data %>%
    filter(!!sym(delegate_column) %in% delegate_values)
  counts <- filtered_data %>%
    group_by(AgeGender, !!sym(delegate_column)) %>%
    summarise(Count = n(), .groups = 'drop')
  totals <- counts %>%
    group_by(AgeGender) %>%
    summarise(Total = sum(Count), .groups = 'drop')
  counts <- counts %>%
    left_join(totals, by = "AgeGender") %>%
    mutate(Proportion = (Count / Total) * 100)
  counts$AgeGender <- factor(counts$AgeGender, levels = age_gender_levels)
  counts[[delegate_column]] <- factor(counts[[delegate_column]], levels = delegate_values, labels = delegate_labels)
  return(counts)
}
# calculate counts and proportions for FinanceDelegate2
finance_counts <- calculate_counts_and_proportions(
  data = test.data.sg,
  delegate_column = "FinanceDelegate2",
  delegate_values = c("Assist-formal", "Assist-Informal"),
  age_gender_levels = c("Younger:Male", "Younger:Female", "Older:Male", "Older:Female"),
  delegate_labels = c("Formal", "Informal")
)
# make and save the proxy plot for FinanceDelegate2
create_proxy_plot_new(
  counts = finance_counts,
  delegate_column = "FinanceDelegate2",
  fill_colors = c("Formal" = "lightgreen", "Informal" = "darkgreen"),
  x_label = "Financial Digital Proxy Practice",
  y_label = "Count across practice type",
  legend_title = "Practice Type",
  output_filename = "financial_proxy_type.jpg"
)

# Figure 12
## Figure 12 in count (SG)
# calculate counts and proportions for MedicalDelegate2
medical_counts <- calculate_counts_and_proportions(
  data = test.data.sg,
  delegate_column = "MedicalDelegate2",
  delegate_values = c("Assist-formal", "Assist-informal"),
  age_gender_levels = c("Younger:Male", "Younger:Female", "Older:Male", "Older:Female"),
  delegate_labels = c("Formal", "Informal")
)
# make and save the proxy plot for MedicalDelegate2
create_proxy_plot_new(
  counts = medical_counts,
  delegate_column = "MedicalDelegate2",
  fill_colors = c("Formal" = "lightblue", "Informal" = "darkblue"),
  x_label = "Medical Digital Proxy Practice",
  y_label = "Count across practice type",
  legend_title = "Practice Type",
  output_filename = "medical_proxy_type.jpg"
)


# Figure 13 (counts of reported methods of logging in to digital account login by informal proxies)
# unique informal (SG) by filtering informal financial and medical delegates
finance_informal<-test.data.sg%>%
  filter(FinanceDelegate2 == "Assist-Informal")
medical_informal<-test.data.sg%>%
  filter(MedicalDelegate2 == "Assist-informal")

# Q5.19 (FinanceLoginWays) (cleanded_data2) (SG)
# recode reasons for a specified column
recode_reasons <- function(data, column_name, new_column_name) {
  data %>%
    mutate(!!new_column_name := recode(!!sym(column_name),
                                       "I know the username and password for their online account(s)" = "haveKnowledge",
                                       "The adult uses their fingerprint (or other biometrics) to log in" = "requireBiometrics",
                                       "The adult logs in with a username and password and I help them (I don't know the username and password)" = "noKnowledge",
                                       "The institution has given me my own separate login and password to help manage their online account(s)" = "proxy account",
                                       .default = !!sym(column_name)))
}
# recode FinanceLoginWays and create a new column names FinanceLoginWays_new
raw_digital_proxy_2 <- recode_reasons(finance_informal, "FinanceLoginWays", "FinanceLoginWays_new")
# data frame about a survey data frame with 118 lines
survey_data2 <- data.frame(
  ID = 1:118,
  Response = raw_digital_proxy_2$FinanceLoginWays_new
)
# function to clean and reshape the survey data
clean_data <- function(data, response_column) {
  data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      username_password = ifelse(str_detect(Response, "haveKnowledge"), "Yes", "No"),
      fingerprint = ifelse(str_detect(Response, "requireBiometrics"), "Yes", "No"),
      donot_know = ifelse(str_detect(Response, "noKnowledge"), "Yes", "No"),
      proxy_account = ifelse(str_detect(Response, "proxy account"), "Yes", "No")
    ) %>%
    group_by(ID) %>%
    na.omit()
}
# clean and reshape the survey data
cleaned_data2 <- clean_data(survey_data2,"Response")
cleaned_data2 <- recode_reasons(cleaned_data2,"Response","Response")
clean_data_new <- function(data, response_column) {
  data <- data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      username_password = ifelse(str_detect(Response, "haveKnowledge"), 1,0),
      fingerprint = ifelse(str_detect(Response, "requireBiometrics"), 1,0),
      donot_know = ifelse(str_detect(Response, "noKnowledge"), 1,0),
      proxy_account = ifelse(str_detect(Response, "proxy account"), 1,0)
    ) %>%
    group_by(ID)
  return(data)
}
# clean and reshape the survey data
cleaned_data2 <- clean_data_new(cleaned_data2, "Response")
# remove all NAs in this dataset
cleaned_data2 <- na.omit(cleaned_data2)

## Q6.38 (DigitalAssistAccessWays) (cleaned_data5) (SG)
# recode DigitalAssistAccessWays and create a new column names DigitalAssistAccessWays_new
raw_digital_proxy_5 <- recode_reasons(medical_informal, "DigitalAssistAccessWays", "DigitalAssistAccessWays_new")
# data frame about a survey data frame with 47 lines
survey_data5 <- data.frame(
  ID = 1:47,
  Response = raw_digital_proxy_5$DigitalAssistAccessWays_new)
cleaned_data5 <- clean_data(survey_data5, "Response")
cleaned_data5 <- recode_reasons(cleaned_data5,"Response","Response")
# clean and reshape the survey data
cleaned_data5 <- clean_data_new(cleaned_data5, "Response")
# remove all NAs in this dataset
cleaned_data5 <- na.omit(cleaned_data5)
# response order for plotting
response_order <- c("haveKnowledge", "noKnowledge", "requireBiometrics")
cleaned_data_finance <- cleaned_data2
cleaned_data_medical <- cleaned_data5
# plot counts of reported methods of logging in to digital account login by informal proxies
plot_delegation_reasons(response_order,cleaned_data2, cleaned_data5, "login.jpg")


# # Figure 14 (count of reported methods of using digital accounts by informal proxies)
# # Q5.20 (FinanceServiceWays) (cleanded_data3) (SG)
# # separated_responses <- medical_informal %>%
# #   separate_rows(DigitalAssistUseWays, sep = "\\|")
# # recode reasons for a specified column
# recode_reasons <- function(data, column_name, new_column_name) {
#   data %>%
#     mutate(!!new_column_name := recode(!!sym(column_name),
#                                        "I offer some help, but the person does it mostly independently" = "partialHelp",
#                                        "I provide ongoing assistance while next to the person" = "ongoingAssistance",
#                                        "I use the person's account on their behalf, while they are present" = "usageWithPresence",
#                                        "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
#                                        "Not Applicable" = "Not Applicable",
#                                        .default = !!sym(column_name)))
# }
# # recode FinanceServiceWays and create a new column names FinanceServiceWays_new
# raw_digital_proxy_3 <- recode_reasons(separated_responses, "FinanceServiceWays", "FinanceServiceWays_new")
# # data frame about a survey data frame with 118 lines
# survey_data3 <- data.frame(
#   ID = 1:nrow(raw_digital_proxy_3),
#   Response = raw_digital_proxy_3$FinanceServiceWays_new
# )
# # function to clean and reshape the survey data
# clean_data <- function(data, response_column) {
#   data %>%
#     separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
#     mutate(
#       guidance = ifelse(str_detect(Response, "partialHelp"), "Yes", "No"),
#       ongoing_assistance = ifelse(str_detect(Response, "ongoingAssistance"), "Yes", "No"),
#       in_person_help = ifelse(str_detect(Response, "usageWithPresence"), "Yes", "No"),
#       unsupervised_help = ifelse(str_detect(Response, "usageWithoutPresence"), "Yes", "No"),
#       Not_Applicable = ifelse(str_detect(Response, "Not Applicable"), "Yes", "No")
#     ) %>%
#     group_by(ID) %>%
#     na.omit()
# }
# # clean and reshape the survey data
# cleaned_data3 <- clean_data(survey_data3,"Response")
# cleaned_data3 <- recode_reasons(cleaned_data3,"Response","Response")
# clean_data_new <- function(data, response_column) {
#   data <- data %>%
#     separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
#     mutate(
#       guidance = ifelse(str_detect(Response, "partialHelp"), 1,0),
#       ongoing_assistance = ifelse(str_detect(Response, "ongoingAssistance"), 1,0),
#       in_person_help = ifelse(str_detect(Response, "usageWithPresence"), 1,0),
#       unsupervised_help = ifelse(str_detect(Response, "usageWithoutPresence"), 1,0),
#       Not_Applicable = ifelse(str_detect(Response, "Not Applicable"), 1,0)
#     ) %>%
#     group_by(ID)
#   return(data)
# }
# cleaned_data3 <- clean_data_new(cleaned_data3, "Response")
# cleaned_data3 <- na.omit(cleaned_data3)
# 
# ## Q6.39 (DigitalAssistUseWays) (cleaned_data6) (SG)
# raw_digital_proxy_6 <- recode_reasons(separated_responses, "DigitalAssistUseWays", "DigitalAssistUseWays_new")
# survey_data6 <- data.frame(
#   ID = 1:59,
#   Response = raw_digital_proxy_6$DigitalAssistUseWays_new)
# cleaned_data6 <- clean_data(survey_data6, "Response")
# cleaned_data6 <- recode_reasons(cleaned_data6, "Response", "Response")
# cleaned_data6 <- clean_data_new(cleaned_data6, "Response")
# cleaned_data6 <- na.omit(cleaned_data6)
# 
# # response order for plotting
# response_order <- c("partialHelp", "ongoingAssistance", "usageWithPresence","usageWithoutPresence","Not Applicable")
# cleaned_data_finance <- cleaned_data3
# cleaned_data_medical <- cleaned_data6
# # plot the count of reported methods of using digital accounts by informal proxies
# plot_delegation_reasons(response_order,cleaned_data_finance, cleaned_data_medical, "Using.jpg")








# Figure 14 (count of reported methods of using digital accounts by informal proxies)
# Q5.20 (FinanceServiceWays) (cleanded_data3) (SG)
# recode reasons for a specified column
# raw_digital_proxy$FinanceServiceWays
raw_digital_proxy<- mutate(finance_informal,
                           FinanceServiceWays_new = recode(FinanceServiceWays,
                                                           "I offer some help, but the person does it mostly independently" = "partialHelp",
                                                           "I provide ongoing assistance while next to the person" = "ongoingAssistance",
                                                           "I use the person's account on their behalf, while they are present" = "usageWithPresence",
                                                           "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
                                                           "Not Applicable" = "Not Applicable",
                                                           .default = FinanceServiceWays
                           ))
FinanceManageReasons_data<-data.frame(raw_digital_proxy$FinanceServiceWays,raw_digital_proxy$FinanceServiceWays_new)

survey_data3 <- data.frame(
  ID = 1:118,
  Response = raw_digital_proxy$FinanceServiceWays_new
)
# function to clean and reshape the survey data
clean_data <- function(data, response_column) {
  data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      guidance = ifelse(str_detect(Response, "partialHelp"), "Yes", "No"),
      ongoing_assistance = ifelse(str_detect(Response, "ongoingAssistance"), "Yes", "No"),
      in_person_help = ifelse(str_detect(Response, "usageWithPresence"), "Yes", "No"),
      unsupervised_help = ifelse(str_detect(Response, "usageWithoutPresence"), "Yes", "No"),
      Not_Applicable = ifelse(str_detect(Response, "Not Applicable"), "Yes", "No")
    ) %>%
    group_by(ID) %>%
    na.omit()
}
cleaned_data3 <- clean_data(survey_data3,"Response")
cleaned_data3<- mutate(cleaned_data3,
                       Response = recode(Response,
                                         "I offer some help, but the person does it mostly independently" = "partialHelp",
                                         "I provide ongoing assistance while next to the person" = "ongoingAssistance",
                                         "I use the person's account on their behalf, while they are present" = "usageWithPresence",
                                         "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
                                         "Not Applicable" = "Not Applicable",
                                         .default = Response
                       ))
# clean and reshape the survey data
clean_data_new <- function(data, response_column) {
  data <- data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      guidance = ifelse(str_detect(Response, "partialHelp"), 1,0),
      ongoing_assistance = ifelse(str_detect(Response, "ongoingAssistance"), 1,0),
      in_person_help = ifelse(str_detect(Response, "usageWithPresence"), 1,0),
      unsupervised_help = ifelse(str_detect(Response, "usageWithoutPresence"), 1,0),
      Not_Applicable = ifelse(str_detect(Response, "Not Applicable"), 1,0)
    ) %>%
    group_by(ID)
  return(data)
}
cleaned_data3 <- clean_data_new(cleaned_data3, "Response")
cleaned_data3 <- na.omit(cleaned_data3)


## Q6.39 (DigitalAssistUseWays) (cleaned_data6) (SG)
# raw_digital_proxy$DigitalAssistUseWays
raw_digital_proxy<- mutate(medical_informal,
                           DigitalAssistUseWays_new = recode(DigitalAssistUseWays,
                                                             "I offer some help, but the person does it mostly independently" = "partialHelp",
                                                             "I provide ongoing assistance while next to the person" = "ongoingAssistance",
                                                             "I use the person's account on their behalf, while they are present" = "usageWithPresence",
                                                             "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
                                                             "Not Applicable" = "Not Applicable",
                                                             .default = DigitalAssistUseWays
                           ))
DigitalAssistUseWays_data<-data.frame(raw_digital_proxy$DigitalAssistUseWays,raw_digital_proxy$DigitalAssistUseWays_new)
survey_data6 <- data.frame(
  ID = 1:47,
  Response = raw_digital_proxy$DigitalAssistUseWays_new)
cleaned_data6 <- clean_data(survey_data6,"Response")

cleaned_data6<- mutate(cleaned_data6,
                       Response = recode(Response,
                                         "I offer some help, but the person does it mostly independently" = "partialHelp",
                                         "I provide ongoing assistance while next to the person" = "ongoingAssistance",
                                         "I use the person's account on their behalf, while they are present" = "usageWithPresence",
                                         "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
                                         "Not Applicable" = "Not Applicable",
                                         .default = Response
                       ))
cleaned_data6 <- clean_data_new(cleaned_data6, "Response")
cleaned_data6 <- na.omit(cleaned_data6)

cleaned_data3$delegateType <- 'Finance'
cleaned_data6$delegateType <- 'Medical'
cleaned_data3 <- na.omit(cleaned_data3)
cleaned_data6 <- na.omit(cleaned_data6)
combined_data <- rbind(cleaned_data3, cleaned_data6)
combined_counts <- combined_data %>%
  group_by(Response, delegateType) %>%
  summarise(Count = n()) %>%
  ungroup()
# response order for plotting
response_order <- c("partialHelp", "ongoingAssistance", "usageWithPresence","usageWithoutPresence","Not Applicable")
combined_counts$Response <- factor(combined_counts$Response, levels = response_order)

# plot the count of reported methods of using digital accounts by informal proxies
plot_delegation_reasons(response_order,cleaned_data3, cleaned_data6, "Using.jpg")


path_to_r_script <- "~/Desktop/new R script with comments/behavior.digital.proxies.csv"
write.csv(behavior.digital.proxies, file = path_to_r_script, row.names = FALSE)
