library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# add more detailed commands for all files

test.data.sg <- test.data.sg %>%
  mutate(DigitalAssistReasons = str_replace_all(DigitalAssistReasons, "\\.", ""))
# head(test.data.sg$DigitalAssistReasons)


# Figure 10
## Q5.18 (FinanceManageReasons) (cleanded_data1) (SG)
test.data.sg = sg_filtered_data[,c('Country','Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital','FinanceManageReasons','FinanceManageReasonsOthers','FinanceLoginWays','FinanceServiceWays','DigitalAssistReasons','DigitalAssistReasonsOthers','DigitalAssistAccessWays','DigitalAssistUseWays','FinanceDelegate1','FinanceDelegate2','MedicalDelegate1','MedicalDelegate2' )]

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
                                       .default = !!sym(column_name)
    ))
  return(data)
}
raw_digital_proxy_1 <- recode_reasons(test.data.sg, "FinanceManageReasons", "FinanceManageReasons_new")

survey_data_1 <- data.frame(
  ID = 1:304,
  Response = raw_digital_proxy_1$FinanceManageReasons_new)

clean_data <- function(data, response_column) {
  data <- data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      not_user_friendly = ifelse(str_detect(!!sym(response_column), "notUserFriendly"), "Yes", "No"),
      difficult = ifelse(str_detect(!!sym(response_column), "taskDifficulty"), "Yes", "No"),
      concerns_data = ifelse(str_detect(!!sym(response_column), "thirdPartyConcerns"), "Yes", "No"),
      unfamiliar = ifelse(str_detect(!!sym(response_column), "lackAppFamiliarity"), "Yes", "No"),
      physical_limitations = ifelse(!!sym(response_column) == "physicalLimitation", "Yes", "No"),
      cognitive_limiation = ifelse(!!sym(response_column) == "cognitiveLimiation", "Yes", "No"),
      no_device = ifelse(!!sym(response_column) == "noDeviceAccess", "Yes", "No"),
      Others = ifelse(!!sym(response_column) == "others", "Yes", "No")
    ) %>%
    group_by(ID)
  return(data)
}

cleaned_data1 <- clean_data(survey_data_1,"Response")
cleaned_data1 <- recode_reasons(cleaned_data1,"Response","Response")

clean_data_new <- function(data, response_column) {
  data <- data %>%
    separate_rows(!!sym(response_column), sep = "\\s*\\|\\s*") %>%
    mutate(
      not_user_friendly = ifelse(str_detect(!!sym(response_column), "notUserFriendly"), 1, 0),
      difficult = ifelse(str_detect(!!sym(response_column), "taskDifficulty"), 1, 0),
      concerns_data = ifelse(str_detect(!!sym(response_column), "thirdPartyConcerns"), 1, 0),
      unfamiliar = ifelse(str_detect(!!sym(response_column), "lackAppFamiliarity"), 1, 0),
      physical_limitations = ifelse(!!sym(response_column) == "physicalLimitation", 1, 0),
      cognitive_limiation = ifelse(!!sym(response_column) == "cognitiveLimiation", 1, 0),
      no_device = ifelse(!!sym(response_column) == "noDeviceAccess", 1, 0),
      Others = ifelse(!!sym(response_column) == "others", 1, 0)
    ) %>%
    group_by(ID)
  return(data)
}

cleaned_data1 <- clean_data_new(cleaned_data1, "Response")
cleaned_data1 <- na.omit(cleaned_data1)

## Q6.37 (DigitalAssistReasons) (cleaned_data4) (SG)
raw_digital_proxy_4 <- recode_reasons(test.data.sg, "DigitalAssistReasons", "DigitalAssistReasons_new")
survey_data4 <- data.frame(
  ID = 1:304,
  Response = raw_digital_proxy_4$DigitalAssistReasons_new)
cleaned_data4 <- clean_data(survey_data4, "Response")
cleaned_data4 <- recode_reasons(cleaned_data4,"Response","Response")

cleaned_data4 <- clean_data_new(cleaned_data4, "Response")
cleaned_data4 <- na.omit(cleaned_data4)

# function for plot
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
response_order <- c("lackAppFamiliarity", "taskDifficulty", "notUserFriendly", "physicalLimitation",
                    "cognitiveLimiation", "thirdPartyConcerns", "noDeviceAccess", "others")
plot_delegation_reasons(response_order, cleaned_data1, cleaned_data4, "manage_account.jpg")


# Figure 11
## Figure 11 in count (SG)
test.data.sg = sg_filtered_data[,c('Country','Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital','FinanceManageReasons','FinanceManageReasonsOthers','FinanceLoginWays','FinanceServiceWays','DigitalAssistReasons','DigitalAssistReasonsOthers','DigitalAssistAccessWays','DigitalAssistUseWays','FinanceDelegate1','FinanceDelegate2','MedicalDelegate1','MedicalDelegate2','FinanceJointAccount')]
test.data.sg$Age_new <- factor(test.data.sg$Age,levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                               labels = c("21","25", "30", "35", "40", "45", "50", "55", "60", "65", "70"))
test.data.sg$Age_new <- ifelse(test.data.sg$Age_new %in% c("60","65", "70"), "Older", "Younger")
test.data.sg$Gender_new <- factor(test.data.sg$Gender,levels = c(1, 2), labels = c("Female","Male"))
test.data.sg$AgeGender <- with(test.data.sg, paste(Age_new, Gender_new, sep=":"))

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

finance_counts <- calculate_counts_and_proportions(
  data = test.data.sg,
  delegate_column = "FinanceDelegate2",
  delegate_values = c("Assist-formal", "Assist-Informal"),
  age_gender_levels = c("Younger:Male", "Younger:Female", "Older:Male", "Older:Female"),
  delegate_labels = c("Formal", "Informal")
)

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
medical_counts <- calculate_counts_and_proportions(
  data = test.data.sg,
  delegate_column = "MedicalDelegate2",
  delegate_values = c("Assist-formal", "Assist-informal"),
  age_gender_levels = c("Younger:Male", "Younger:Female", "Older:Male", "Older:Female"),
  delegate_labels = c("Formal", "Informal")
)

create_proxy_plot_new(
  counts = medical_counts,
  delegate_column = "MedicalDelegate2",
  fill_colors = c("Formal" = "lightblue", "Informal" = "darkblue"),
  x_label = "Medical Digital Proxy Practice",
  y_label = "Count across practice type",
  legend_title = "Practice Type",
  output_filename = "medical_proxy_type.jpg"
)


# Figure 13
# unique informal (SG)
finance_informal<-test.data.sg%>%
  filter(FinanceDelegate2 == "Assist-Informal")
medical_informal<-test.data.sg%>%
  filter(MedicalDelegate2 == "Assist-informal")

# Q5.19 (FinanceLoginWays) (cleanded_data2) (SG)
recode_reasons <- function(data, column_name, new_column_name) {
  data %>%
    mutate(!!new_column_name := recode(!!sym(column_name),
                                       "I know the username and password for their online account(s)" = "haveKnowledge",
                                       "The adult uses their fingerprint (or other biometrics) to log in" = "requireBiometrics",
                                       "The adult logs in with a username and password and I help them (I don't know the username and password)" = "noKnowledge",
                                       "The institution has given me my own separate login and password to help manage their online account(s)" = "proxy account",
                                       .default = !!sym(column_name)))
}
raw_digital_proxy_2 <- recode_reasons(finance_informal, "FinanceLoginWays", "FinanceLoginWays_new")

survey_data2 <- data.frame(
  ID = 1:118,
  Response = raw_digital_proxy_2$FinanceLoginWays_new
)

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

cleaned_data2 <- clean_data(survey_data_2,"Response")
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
cleaned_data2 <- clean_data_new(cleaned_data2, "Response")
cleaned_data2 <- na.omit(cleaned_data2)

## Q6.38 (DigitalAssistAccessWays) (cleaned_data5) (SG)
raw_digital_proxy_5 <- recode_reasons(medical_informal, "DigitalAssistAccessWays", "DigitalAssistAccessWays_new")
survey_data5 <- data.frame(
  ID = 1:47,
  Response = raw_digital_proxy_5$DigitalAssistAccessWays_new)
cleaned_data5 <- clean_data(survey_data5, "Response")
cleaned_data5 <- recode_reasons(cleaned_data5,"Response","Response")

cleaned_data5 <- clean_data_new(cleaned_data5, "Response")
cleaned_data5 <- na.omit(cleaned_data5)

response_order <- c("haveKnowledge", "noKnowledge", "requireBiometrics")
plot_delegation_reasons(response_order,cleaned_data2, cleaned_data5, "login.jpg")


# Figure 14
# Q5.20 (FinanceServiceWays) (cleanded_data3) (SG)
recode_reasons <- function(data, column_name, new_column_name) {
  data %>%
    mutate(!!new_column_name := recode(!!sym(column_name),
                                       "I offer some help, but the person does it mostly independently" = "partialHelp",
                                       "I provide ongoing assistance while next to the person" = "ongoingAssistance",
                                       "I use the person's account on their behalf, while they are present" = "usageWithPresence",
                                       "I use the person's account on their behalf, even when they are not present" = "usageWithoutPresence",
                                       "Not Applicable" = "Not Applicable",
                                       .default = !!sym(column_name)))
}
raw_digital_proxy_3 <- recode_reasons(finance_informal, "FinanceServiceWays", "FinanceServiceWays_new")

survey_data3 <- data.frame(
  ID = 1:118,
  Response = raw_digital_proxy_3$FinanceServiceWays_new
)

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

cleaned_data3 <- clean_data(survey_data_3,"Response")
cleaned_data3 <- recode_reasons(cleaned_data3,"Response","Response")


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
raw_digital_proxy_6 <- recode_reasons(medical_informal, "DigitalAssistUseWays", "DigitalAssistUseWays_new")
survey_data6 <- data.frame(
  ID = 1:47,
  Response = raw_digital_proxy_6$DigitalAssistAccessWays_new)
cleaned_data6 <- clean_data(survey_data6, "Response")
cleaned_data6 <- recode_reasons(cleaned_data6,"Response","Response")

cleaned_data6 <- clean_data_new(cleaned_data6, "Response")
cleaned_data6 <- na.omit(cleaned_data6)

response_order <- c("partialHelp", "ongoingAssistance", "usageWithPresence","usageWithoutPresence","Not Applicable")
plot_delegation_reasons(response_order,cleaned_data3, cleaned_data6, "login.jpg")




