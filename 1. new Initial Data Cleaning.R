library(MASS)
library(readr)
library(tidyverse)
library(readr)
library(ggplot2)
library(corrplot)
library(dplyr)

dat <- read_csv("~/OneDrive - University of Toronto/Digital Proxy Practicum/Cleaned data/cleaned_combined.csv")
my.dat <- dat

#define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

# define function to transform factor levels and labels
transform_factor <- function(data, column_name, na = NULL, levels, labels, ordered = FALSE, convert_to_numeric = FALSE) {
  if (!is.null(na)) {
    data[[column_name]][is.na(data[[column_name]])] <- na
  }
  data[[column_name]] <- factor(data[[column_name]], levels = levels, labels = labels, ordered = ordered)
  if (convert_to_numeric) {
    data[[column_name]] <- as.numeric(as.character(data[[column_name]]))
  }
  return(data)}

# define function to transform ordered factor to numeric
transform_order_to_numeric <- function(data, column_name, ordering) {
  # create a factor with specified levels
  orderFactor <- factor(data[[column_name]], levels = ordering)
  # convert factor to numeric
  data[[column_name]] <- as.numeric(orderFactor)
  return(data)}
# use function to filter the country: US or SG
filterCountryData <- function(xyz) { 
  test<-my.dat%>%
    filter(Country==xyz)%>%
    return(test)
}
digital_proxyUS <- filterCountryData("US")
digital_proxySG <- filterCountryData("SG")

# Factorize Demographics -------------------------------------------------
my.dat <- transform_factor(my.dat, "Country",levels = c("US", "SG"), labels = c("US","SG"))
my.dat <- transform_factor(my.dat,"Marital",na = "3",levels = c("Unmarried", "Married", "Div/Sep", "Widowed"),labels = c("Unmarried", "Married", "Once Married", "Once Married"))
my.dat <- transform_order_to_numeric(my.dat, "Age", c("21", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70"))
my.dat <- transform_factor(my.dat, "Housing",levels = c("low-income housing", "apartment", "familyhouse"),labels = c("0", "1", "2"), ordered = TRUE, convert_to_numeric = TRUE)
my.dat <- transform_order_to_numeric(my.dat, "Income", c("<$30k", "$30k", "$50k", "$100k", "$150k", ">$200k"))
my.dat <- transform_order_to_numeric(my.dat, "Gender", c("Female", "Male"))
my.dat <- transform_factor(my.dat,"Ethnicity",levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"),labels = c("Majority", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Majority", "Underrepresented", "Underrepresented", "Underrepresented"),convert_to_numeric = TRUE)
# my.dat <- transform_factor(my.dat,"Ethnicity",levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"),labels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"),convert_to_numeric = TRUE)
my.dat <- transform_factor(my.dat,"Religion",levels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic", "Buddhism", "Catholicism", "No Religion", "Islam", "Christianity", "Taoism", "Hinduism"),labels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic", "Buddhism", "Catholicism", "No Religion", "Islam", "Christianity", "Taoism", "Hinduism"),convert_to_numeric = TRUE)
my.dat <- transform_factor(my.dat,"Education",levels = c("Below", "High", "Degree and above"),labels = c("< High School", "High School", "Degree and above"),ordered = TRUE,convert_to_numeric = TRUE)
my.dat <- transform_factor(my.dat,"Employment",levels = c("Disabled", "Retired", "Student", "Unemployed", "Full-time", "Part-time", "Self"),labels = c("Unemployed", "Unemployed", "Unemployed", "Unemployed", "Employed", "Employed", "Employed"),ordered = TRUE,convert_to_numeric = TRUE)
my.dat <- transform_order_to_numeric(my.dat, "Siblings", c("0", "1", "2", "3", ">4"))
my.dat <- transform_order_to_numeric(my.dat, "SiblingsOrder", c("Youngest","Middle","Oldest"))


# Factorize Physical Help -------------------------------------------------
my.dat <- transform_factor(my.dat,"HelpImmediateFam",levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25",">30"),labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25","31"),convert_to_numeric = TRUE)
# in relatives
my.dat <- transform_factor(my.dat,"HelpRelatives",levels = c("0","1","2","3","4","5","6","9","10","16","20","29",">30"),labels = c("0","1","2","3","4","5","6","9","10","16","20","29","31"),convert_to_numeric = TRUE)
# in Friends
my.dat <- transform_factor(my.dat,"HelpFriends",levels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30",">30",">=100"),labels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30","31","101"),convert_to_numeric = TRUE)
# in Pass Away
my.dat <- transform_factor(my.dat,"HelpPassAway",na = "-1",  # handle NA
                           levels = c("-1","0","1","2","3",">3"),labels = c("-1","0","1","2","3","4"),convert_to_numeric = TRUE)

# Factorize Financial Help -------------------------------------------------
my.dat <- transform_factor(my.dat,"FinanceHelpImmediateFam",levels = c("0","1","2","3","4","5","6","10","12","18","20",">30"),labels = c("0","1","2","3","4","5","6","10","12","18","20","31"),convert_to_numeric = TRUE)
# in relatives
my.dat <- transform_factor(my.dat,"FinanceHelpRelatives",levels = c("0","1","2","3","5","6","7","8","10","20","30",">30"),labels = c("0","1","2","3","5","6","7","8","10","20","30","31"),convert_to_numeric = TRUE)
# in Friends
my.dat <- transform_factor(my.dat,"FinanceHelpFriends",levels = c("0","1","2","3","4","5","6","7","9","10","12","15","20",">30",">100"),labels = c("0","1","2","3","4","5","6","7","9","10","12","15","20","31","101"),convert_to_numeric = TRUE)
# in Pass Away
my.dat <- transform_factor(my.dat,"FinanceHelpPassAway", na = "-1",  # handle NA
                           levels = c("-1","0","1","2","3",">3"),labels = c("-1","0","1","2","3","4"),convert_to_numeric = TRUE)

# Factorize Medical Help -------------------------------------------------
my.dat <- transform_factor(my.dat,"MedicalHelpImmediateFam",levels = c("0","1","2","3","4","5","6","10","12","18","20",">30",">100"),labels = c("0","1","2","3","4","5","6","10","12","18","20","31","101"),convert_to_numeric = TRUE)
# in relatives
my.dat <- transform_factor(my.dat,"MedicalHelpRelatives",levels = c("0","1","2","3","4","5","6","7","10","20",">30"),labels = c("0","1","2","3","4","5","6","7","10","20","31"),convert_to_numeric = TRUE)
# in Friends
my.dat <- transform_factor(my.dat,"MedicalHelpFriends",levels = c("0","1","2","3","4","5","6","7","10","15","20","26",">30",">=100"),labels = c("0","1","2","3","4","5","6","7","10","15","20","26","31","101"),convert_to_numeric = TRUE)
# in Pass Away
my.dat <- transform_factor(my.dat,"MedicalHelpPassAway",na = "-1",  # handle NA
                           levels = c("-1","0","1","2","3",">3"),labels = c("-1","0","1","2","3","4"), convert_to_numeric = TRUE)

# Factorize IADL -------------------------------------------------
transform_IADL_ADL <- function(data, columns, levels, labels) {
  for (col in columns) {
    data[[col]] <- factor(data[[col]], levels = levels, labels = labels)
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
  return(data)}
# in Help with Food, Housekeeping, Laundry, Transport, Medicine, Finance, Digital Service
IADL_columns <- c("HelpFood", "HelpHousekeep", "HelpLaundry", "HelpTransport","HelpMed","HelpFinance","HelpDigital")
levels_list_IADL <- list(c("Never", "Yearly", "Monthly", "Weekly", "Daily"))
labels_list_IADL <- list(c("0", "1", "2", "3", "4"))
my.dat <- transform_IADL_ADL(my.dat, IADL_columns, levels_list_IADL, labels_list_IADL)

# Factorize IADL(future) -------------------------------------------------
transform_IADL_future <- function(data, column_names, na = NULL, levels, labels, convert_to_numeric = FALSE) {
  for (col in column_names) {
    if (!is.null(na)) {
      data[[col]][is.na(data[[col]])] <- na
    }
    data[[col]] <- factor(data[[col]], levels = levels, labels = labels)
    if (convert_to_numeric) {
      data[[col]] <- as.numeric(as.character(data[[col]]))}
  }
  return(data)}
# in Help with Food, Housekeeping, Laundry, Transport, Medicine, Finance, Digital Service
columns_IADL_future <- c("HelpFoodFuture", "HelpHousekeepFuture", "HelpLaundryFuture","HelpTransportFuture","HelpMedFuture","HelpFinanceFuture","HelpDigitalFuture")
levels_list_future <- c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely")
labels_list_future <- c("-1","0","1","2","3","4")
my.dat <- transform_IADL_future(my.dat, columns_IADL_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)

# Factorize ADL -------------------------------------------------
# function to transform ADL variables
# in ADL Bathing, Grooming, Toileting, Transferring, Incontinence, Feeding
ADL_columns <- c("ADLBath","ADLGroom", "ADLToilet", "ADLTransfer","ADLIncontinence","ADLFeed")
levels_list <- list(c("Never","Yearly","Monthly","Weekly","Daily"))
labels_list <- list(c("0","1","2","3","4"))
my.dat <- transform_IADL_ADL(my.dat, ADL_columns, levels_list, labels_list)

# Factorize Financial Task -------------------------------------------------
# in Help with Decision Support, Finance Action
financal_columns <- c("FinanceDecisionSupport","FinanceAction")
my.dat <- transform_IADL_ADL(my.dat, financal_columns, levels_list, labels_list)
# in Help with Finance LPA
my.dat <- transform_factor(my.dat, "FinanceLPA", levels = c("No", "Yes"), labels = c("0", "1"), convert_to_numeric = TRUE)
# in Help with Finance LPA Personal Use
my.dat <- transform_factor(my.dat,"FinanceLPAUsePersonal",na = "-1",levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),labels = c("-1","0","1","2","3","4"),convert_to_numeric = TRUE)
# in Help with Finance LPA Property Use
my.dat <- transform_factor(my.dat,"FinanceLPAUseProperty",na = "-1",levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),labels = c("-1","0","1","2","3","4"),convert_to_numeric = TRUE)

# Factorize Financial Task (Future)-------------------------------------------------
# in Help with Decision Support, Finance Action
columns_financial_future <- c("FinanceDecisionSupportFuture", "FinanceActionFuture")
levels_list_future <- c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely")
labels_list_future <- c("-1","0","1","2","3","4")
my.dat <- transform_IADL_future(my.dat, columns_financial_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)

# Factorize Medical Task Low Stake -------------------------------------------------
# in Help with Making Medical Appt, in Accompanying with Medical Appt, Help with Taking Medicine, Help with Overseeing Treatment
low_stake_columns <- c("MedicalMakeAppt","MedicalAccompanyAppt","MedicalTakeMeds","MedicalSeeTreatment")
my.dat <- transform_IADL_ADL(my.dat, low_stake_columns, levels_list, labels_list)

# Factorize Medical Task Low Stake (future) -------------------------------------------------
columns_financial_future <- c("MedicalMakeApptFuture", "MedicalAccompanyApptFuture","MedicalTakeMedsFuture","MedicalSeeTreatmentFuture")
levels_list_future <- c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely")
labels_list_future <- c("-1","0","1","2","3","4")
my.dat <- transform_IADL_future(my.dat, columns_financial_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)

# Factorize Medical Task High Stake -------------------------------------------------
# Making medical decisions, Quality of life, Medical Care - end of life, Flexibility for Surrogate in Making Decisions
high_stake_columns <- c("MedicalDecisionPerson","MedicalDecisionDoctor","MedicalSignPapers",
                        "MedicalWorthPerson","MedicalWorthDoctor","MedicalWorthFamily",
                        "MedicalEndPerson","MedicalEndDoctor","MedicalEndFamily",
                        "MedicalFlexibilityPerson","MedicalFlexibilityDoctor","MedicalFlexibilityFamily","MedicalDoctorQuestions")
levels_list_high_stake <- list(c("No, because I didn't need to","No, but I should have","Yes"))
labels_list_high_stake <- list(c("0","1","2"))
my.dat <- transform_IADL_ADL(my.dat, high_stake_columns, levels_list_high_stake, labels_list_high_stake)

# Function to calculate row sums for specified columns and round the result to 2 digits
calculate_and_process_sums <- function(data, column_groups) {
  for (group_name in names(column_groups)) {
    data[[group_name]] <- rowSums(data[, column_groups[[group_name]]])
  }
  return(data)}
column_groups <- list(
  MedicalDecision = c("MedicalDecisionPerson", "MedicalDecisionDoctor", "MedicalSignPapers"),
  MedicalWorth = c("MedicalWorthPerson", "MedicalWorthDoctor", "MedicalWorthFamily"),
  MedicalEnd = c("MedicalEndPerson", "MedicalEndDoctor", "MedicalEndFamily"),
  MedicalSurrogate = c("MedicalFlexibilityPerson", "MedicalFlexibilityDoctor", "MedicalFlexibilityFamily", "MedicalDoctorQuestions"),
  MedicalHighStake = c("MedicalDecision", "MedicalWorth", "MedicalEnd", "MedicalSurrogate")
)
my.dat <- calculate_and_process_sums(my.dat, column_groups)
my.dat$MedicalHighStake <- scale_values(my.dat$MedicalHighStake)
my.dat$MedicalHighStake <- round(my.dat$MedicalHighStake, digits = 2)

# in Managing with Caregiver Account (Num)
unique(my.dat$DigitalCaregiverAccountNum)
my.dat <- transform_factor(my.dat,"DigitalCaregiverAccountNum",levels = c("0","1","2","3","4","5","8","10","12","17","23",">30",">=100"),labels = c("0","1","2","3","4","5","8","10","12","17","23","31","100"),convert_to_numeric = TRUE)

# GROUPING DIGITAL DELEGATE HERE >> IMPORTANT-------------------------------------------------
#Q5.15: I have a joint account (note: the raw values can be retrieved via 'FinanceJointAccount_raw' variable)
my.dat <- transform_factor(my.dat,"FinanceJointAccount",levels = c("No","Yes","Unsure"),labels = c("0","1","0"),convert_to_numeric = TRUE)
#Q5.17: I have helped with digital financial access
my.dat <- transform_factor(my.dat,"FinanceDigitalAssistance",levels = c("No","Yes"),labels = c("0","1"),convert_to_numeric = TRUE)
#Q5.19: Own a separate account (note: the raw values can be retrieved via 'FinanceLoginWays' variable)
my.dat <- transform_factor(my.dat,"FinanceLoginWays_institution",na = "2",  # handle NA
                           levels = c("No","Yes"),labels = c("0","1"),convert_to_numeric = TRUE)
#Q6.33: I have a joint account
my.dat <- transform_factor(my.dat,"DigitalCaregiverAccount",levels = c("Unsure","No, but I should have","Yes"),labels = c("0","0","1"),convert_to_numeric = TRUE)
#Q6.36: I have assisted another adult with digital medical services
my.dat <- transform_factor(my.dat,"DigitalAssistMedical",levels = c("No","Yes"),labels = c("0","1"),convert_to_numeric = TRUE)
#Q6.38: My own separate account(note: the raw values can be retrieved via 'DigitalAssistAccessWays' variable)
my.dat <- transform_factor(my.dat,"DigitalAssistAccessWays_institution",na = "2",  # handle NA
                           levels = c("No","Yes"),labels = c("0","1"),convert_to_numeric = TRUE)

# Function to filter out a specific country from the dataset
filter_country_data <- function(data, country) {
  return(subset(data, !(Country == country)))} # use subset make the data excluding the specified country

# function to repeat subset operation
remove_outliers <- function(data, conditions, outlier_values) {
  for (i in seq_along(conditions)) {
    data <- subset(data, !(data[[conditions[i]]] %in% outlier_values[[i]]))}
  return(data)}
# function to repeat rowSums,scale_values,round
calculate_and_process_scores <- function(data, columns, scale = TRUE, digits = 2) {
  scores <- rowSums(data[, columns], na.rm = TRUE)
  if (scale) {
    scores <- scale_values(scores)}
  scores <- round(scores, digits = digits)
  return(scores)}

convert_to_factors <- function(data, columns) {
  for (col in columns) {
    data[[col]] <- as.factor(data[[col]])}
  return(data)}

process_data <- function(data, country) {
  filtered_data <- filter_country_data(data, country)
  # remove outliers
  conditions <- c("HelpFriends", "FinanceHelpFriends", "MedicalHelpImmediateFam", "MedicalHelpFriends", "HelpFriends", "FinanceHelpFriends", "MedicalHelpImmediateFam", "MedicalHelpFriends", "FinanceJointAccount", "Marital")
  outlier_values <- list("101", "101", "101", "101", "31", "31", "31", "31", "Exclude", "3")
  filtered_data <- remove_outliers(filtered_data, conditions, outlier_values)
  # adjust Marital factor levels
  filtered_data$Marital <- factor(filtered_data$Marital, levels = c("Once Married", "Unmarried", "Married"))
  # dataframe with only demographic predictors
  test_data <- filtered_data[, c('Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital')]
  # calculate IADL score
  IADL_columns <- c("HelpTransport","HelpMed","HelpFinance","HelpFood","HelpLaundry","HelpHousekeep")
  test_data$IADL <- calculate_and_process_scores(filtered_data, IADL_columns, scale = TRUE)
  # calculate ADL score
  ADL_columns <- c("ADLBath","ADLFeed","ADLGroom","ADLIncontinence","ADLToilet","ADLTransfer")
  test_data$ADL <- calculate_and_process_scores(filtered_data, ADL_columns, scale = TRUE)
  # calculate low-stake medical score
  lowstake_columns <- c("MedicalMakeAppt","MedicalAccompanyAppt","MedicalTakeMeds","MedicalSeeTreatment")
  test_data$lowstakeMed <- calculate_and_process_scores(filtered_data, lowstake_columns, scale = FALSE)
  # calculate high-stake medical score
  highstake_columns <- c("MedicalDecisionPerson","MedicalDecisionDoctor","MedicalSignPapers","MedicalWorthPerson",
                         "MedicalWorthDoctor","MedicalWorthFamily","MedicalEndPerson","MedicalEndFamily",
                         "MedicalFlexibilityPerson","MedicalFlexibilityDoctor","MedicalFlexibilityFamily","MedicalDoctorQuestions")
  test_data$highstakeMed <- calculate_and_process_scores(filtered_data, highstake_columns, scale = FALSE)
  factor_columns <- c("FinanceDelegate1", "FinanceDelegate2", "MedicalDelegate1", "MedicalDelegate2")
  test_data <- convert_to_factors(test_data, factor_columns)
  return(test_data)}
filtered.dat <- my.dat
test.data.sg <- process_data(filtered.dat,"US")
test.data.us <- process_data(filtered.dat,"SG")

path_to_r_script <- "~/Desktop/new R script with comments/my.dat.csv"
write.csv(my.dat, file = path_to_r_script, row.names = FALSE)
