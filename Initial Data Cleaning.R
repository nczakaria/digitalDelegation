library(MASS)
library(readr)
library(tidyverse)
library(readr)
library(ggplot2)
library(corrplot)
library(dplyr)

dat <- read_csv("~/OneDrive - University of Toronto/Digital Proxy Practicum/Cleaned data/cleaned_combined_housing.csv")
my.dat <- dat

################---------------------------------------------------
# FUNCTIONS ONLY
# scaling, factorDemographic, factorHelp, filterData
################---------------------------------------------------

# define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

# What is this function doing? demographic consists of Country, Maritial Status, Age, Gender, Religion, Education, Employment, Siblings, Siblings Order, Housing, Ethnicity
factorDemographics <- function(x.dataset, countryCode, plotting){
  
  x.dataset$Country <- factor(x.dataset$Country, levels = c("US","SG"), labels = c("US","SG"))
  
  x.dataset$Marital[is.na(x.dataset$Marital)] = "3"
  x.dataset$Marital <- factor(x.dataset$Marital,
                              levels = c("Unmarried","Married","Div/Sep","Widowed"),
                              labels = c("Unmarried","Married","Once Married","Once Married"))
  x.dataset$Marital <- relevel(x.dataset$Marital, ref = "Once Married")
  
  ordering <- c("21","25", "30", "35", "40", "45", "50", "55", "60", "65", "70")
  orderFactor <- factor(x.dataset$Age, levels = ordering)
  x.dataset$Age <- as.numeric(orderFactor)
  
  ordering <- c("<$30k", "$30k", "$50k", "$100k", "$150k", ">$200k")
  orderFactor <- factor(x.dataset$Income, levels = ordering)
  x.dataset$Income <- as.numeric(orderFactor)
  
  x.dataset$Gender <- as.factor(x.dataset$Gender)
  x.dataset$Gender <- as.numeric(x.dataset$Gender)
  
  x.dataset$Religion <- factor(x.dataset$Religion, 
                               levels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic","Buddhism","Catholicism","No Religion","Islam","Christianity","Taoism","Hinduism"),
                               labels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic","Buddhism","Catholicism","No Religion","Islam","Christianity","Taoism","Hinduism"))
  x.dataset$Religion <- as.factor(as.character(x.dataset$Religion))
  
  x.dataset$Education <- factor(x.dataset$Education, 
                                levels = c("Below", "High", "Degree and above"),
                                labels = c("< High School", "High School", "Degree and above"), ordered = TRUE)
  ordering <- c("< High School", "High School", "Degree and above")
  orderFactor <- factor(x.dataset$Education, levels = ordering)
  x.dataset$Education <- as.numeric(orderFactor)
  
  x.dataset$Employment <- factor(x.dataset$Employment, 
                                 levels = c("Disabled","Retired","Student","Unemployed","Full-time","Part-time","Self"),
                                 labels = c("Unemployed","Unemployed","Unemployed","Unemployed","Employed","Employed","Employed"), ordered = TRUE)
  x.dataset$Employment <- as.factor(as.character(x.dataset$Employment))
  x.dataset$Employment <- as.numeric(x.dataset$Employment)
  
  ordering <- c("0","1","2","3",">4")
  orderFactor <- factor(x.dataset$Siblings, levels = ordering)
  x.dataset$Siblings <- as.numeric(orderFactor)
  
  ordering <- c("Youngest","Middle","Oldest")
  orderFactor <- factor(x.dataset$SiblingsOrder, levels = ordering)
  x.dataset$SiblingsOrder <- as.numeric(orderFactor)
  
  if(countryCode == "US")
  {
    x.dataset$Housing <- factor(x.dataset$Housing, 
                                levels = c("low-income housing", "apartment", "familyhouse"),
                                labels = c("0", "1", "2"), ordered = TRUE)
    x.dataset$Housing <- as.factor(as.character(x.dataset$Housing))
    x.dataset$Housing <- as.numeric(x.dataset$Housing)
  }else
  {
    x.dataset$Housing <- factor(x.dataset$Housing, 
                                levels = c("HDB 1- or 2-Room Flat", "HDB 3-Room Flat", "HDB 4-Room Flat", "HDB 5-Room or Executive Flat", "Condominium or other apartment types", "Landed Property"),
                                labels = c("0", "1", "2", "3", "4", "5"), ordered = TRUE)
    x.dataset$Housing <- as.factor(as.character(x.dataset$Housing))
    x.dataset$Housing <- as.numeric(x.dataset$Housing)
  }

  if(plotting==TRUE)
  {
    x.dataset$Ethnicity <- factor(x.dataset$Ethnicity, levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"), labels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"))
    x.dataset$Ethnicity <- as.factor(as.character(x.dataset$Ethnicity))
  }
  else
  {
    x.dataset$Ethnicity <- factor(x.dataset$Ethnicity, 
                                  levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"),
                                  labels = c("Majority", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Majority", "Underrepresented", "Underrepresented", "Underrepresented"))
    x.dataset$Ethnicity <- as.factor(as.character(x.dataset$Ethnicity))
    x.dataset$Ethnicity <- as.numeric(x.dataset$Ethnicity)
  }

  x.dataset <- subset(x.dataset, (Country==countryCode))
  
  return(x.dataset)
}

# This function factor physical help, financial help, medical help, helping in IADL and ADL, helping in Financial Tasks and Medical Tasks (low-stake & high stake activities)
factorHelp <- function(x.dataset)
{
  # Factorize Physical Help -------------------------------------------------
  x.dataset$HelpImmediateFam = factor(x.dataset$HelpImmediateFam, 
                                   levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25",">30"), 
                                   labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25","31"))
  x.dataset$HelpImmediateFam <- as.numeric(as.character(x.dataset$HelpImmediateFam))
  
  # in relatives
  x.dataset$HelpRelatives = factor(x.dataset$HelpRelatives, 
                                levels = c("0","1","2","3","4","5","6","9","10","16","20","29",">30"), 
                                labels = c("0","1","2","3","4","5","6","9","10","16","20","29","31"))
  x.dataset$HelpRelatives <- as.numeric(as.character(x.dataset$HelpRelatives))
  
  # in Friends
  x.dataset$HelpFriends = factor(x.dataset$HelpFriends, 
                              levels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30",">30",">=100"), 
                              labels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30","31","101"))
  x.dataset$HelpFriends <- as.numeric(as.character(x.dataset$HelpFriends))
  
  # in Pass Away
  x.dataset$HelpPassAway[is.na(x.dataset$HelpPassAway)] = -1
  x.dataset$HelpPassAway = factor(x.dataset$HelpPassAway, 
                               levels = c("-1","0","1","2","3",">3"), 
                               labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpPassAway <- as.numeric(as.character(x.dataset$HelpPassAway))
  
  # Factorize Financial Help -------------------------------------------------
  x.dataset$FinanceHelpImmediateFam = factor(x.dataset$FinanceHelpImmediateFam, 
                                          levels = c("0","1","2","3","4","5","6","10","12","18","20",">30"), 
                                          labels = c("0","1","2","3","4","5","6","10","12","18","20","31"))
  x.dataset$FinanceHelpImmediateFam <- as.numeric(as.character(x.dataset$FinanceHelpImmediateFam))
  
  # in relatives
  x.dataset$FinanceHelpRelatives = factor(x.dataset$FinanceHelpRelatives, 
                                       levels = c("0","1","2","3","5","6","7","8","10","20","30",">30"), 
                                       labels = c("0","1","2","3","5","6","7","8","10","20","30","31"))
  x.dataset$FinanceHelpRelatives <- as.numeric(as.character(x.dataset$FinanceHelpRelatives))
  
  # in Friends
  x.dataset$FinanceHelpFriends = factor(x.dataset$FinanceHelpFriends, 
                                     levels = c("0","1","2","3","4","5","6","7","9","10","12","15","20",">30",">100"), 
                                     labels = c("0","1","2","3","4","5","6","7","9","10","12","15","20","31","101"))
  x.dataset$FinanceHelpFriends <- as.numeric(as.character(x.dataset$FinanceHelpFriends))
  
  
  # in Pass Away
  x.dataset$FinanceHelpPassAway[is.na(x.dataset$FinanceHelpPassAway)] = -1
  x.dataset$FinanceHelpPassAway = factor(x.dataset$FinanceHelpPassAway, 
                                      levels = c("-1","0","1","2","3",">3"), 
                                      labels = c("-1","0","1","2","3","4"))
  x.dataset$FinanceHelpPassAway <- as.numeric(as.character(x.dataset$FinanceHelpPassAway))
  
  # Factorize Medical Help -------------------------------------------------
  x.dataset$MedicalHelpImmediateFam = factor(x.dataset$MedicalHelpImmediateFam, 
                                          levels = c("0","1","2","3","4","5","6","10","12","18","20",">30",">100"), 
                                          labels = c("0","1","2","3","4","5","6","10","12","18","20","31","101"))
  x.dataset$MedicalHelpImmediateFam <- as.numeric(as.character(x.dataset$MedicalHelpImmediateFam))
  
  # in relatives
  x.dataset$MedicalHelpRelatives = factor(x.dataset$MedicalHelpRelatives, 
                                       levels = c("0","1","2","3","4","5","6","7","10","20",">30"), 
                                       labels = c("0","1","2","3","4","5","6","7","10","20","31"))
  x.dataset$MedicalHelpRelatives <- as.numeric(as.character(x.dataset$MedicalHelpRelatives))
  
  # in Friends
  x.dataset$MedicalHelpFriends = factor(x.dataset$MedicalHelpFriends, 
                                     levels = c("0","1","2","3","4","5","6","7","10","15","20","26",">30",">=100"), 
                                     labels = c("0","1","2","3","4","5","6","7","10","15","20","26","31","101"))
  x.dataset$MedicalHelpFriends <- as.numeric(as.character(x.dataset$MedicalHelpFriends))
  
  # in Pass Away
  x.dataset$MedicalHelpPassAway[is.na(x.dataset$MedicalHelpPassAway)] = -1
  x.dataset$MedicalHelpPassAway = factor(x.dataset$MedicalHelpPassAway, 
                                      levels = c("-1","0","1","2","3",">3"), 
                                      labels = c("-1","0","1","2","3","4"))
  x.dataset$MedicalHelpPassAway <- as.numeric(as.character(x.dataset$MedicalHelpPassAway))
  
  # Factorize IADL -------------------------------------------------
  # in Help with Food
  x.dataset$HelpFood = factor(x.dataset$HelpFood,
                           levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                           labels = c("0","1","2","3","4"))
  x.dataset$HelpFood <- as.numeric(as.character(x.dataset$HelpFood))
  
  # in Help with Housekeeping
  x.dataset$HelpHousekeep = factor(x.dataset$HelpHousekeep,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
  x.dataset$HelpHousekeep <- as.numeric(as.character(x.dataset$HelpHousekeep))
  
  # in Help with Laundry
  x.dataset$HelpLaundry = factor(x.dataset$HelpLaundry,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
  x.dataset$HelpLaundry <- as.numeric(as.character(x.dataset$HelpLaundry))
  
  # in Help with Transport
  x.dataset$HelpTransport = factor(x.dataset$HelpTransport,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
  x.dataset$HelpTransport <- as.numeric(as.character(x.dataset$HelpTransport))
  
  # in Help with Medicine
  x.dataset$HelpMed = factor(x.dataset$HelpMed,
                          levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                          labels = c("0","1","2","3","4"))
  x.dataset$HelpMed <- as.numeric(as.character(x.dataset$HelpMed))
  
  # in Help with Finance
  x.dataset$HelpFinance = factor(x.dataset$HelpFinance,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
  x.dataset$HelpFinance <- as.numeric(as.character(x.dataset$HelpFinance))
  
  # in Help with Digital Service
  x.dataset$HelpDigital = factor(x.dataset$HelpDigital,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
  x.dataset$HelpDigital <- as.numeric(as.character(x.dataset$HelpDigital))
  
  # Factorize IADL(future) -------------------------------------------------
  # in Help with Food
  x.dataset$HelpFoodFuture[is.na(x.dataset$HelpFoodFuture)] = "-1"
  x.dataset$HelpFoodFuture = factor(x.dataset$HelpFoodFuture,
                                 levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                 labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpFoodFuture <- as.numeric(as.character(x.dataset$HelpFoodFuture))
  
  # in Help with Housekeeping
  x.dataset$HelpHousekeepFuture[is.na(x.dataset$HelpHousekeepFuture)] = "-1"
  x.dataset$HelpHousekeepFuture = factor(x.dataset$HelpHousekeepFuture,
                                      levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                      labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpHousekeepFuture <- as.numeric(as.character(x.dataset$HelpHousekeepFuture))
  
  # in Help with Laundry
  x.dataset$HelpLaundryFuture[is.na(x.dataset$HelpLaundryFuture)] = "-1"
  x.dataset$HelpLaundryFuture = factor(x.dataset$HelpLaundryFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpLaundryFuture <- as.numeric(as.character(x.dataset$HelpLaundryFuture))
  
  # in Help with Transport
  x.dataset$HelpTransportFuture[is.na(x.dataset$HelpTransportFuture)] = "-1"
  x.dataset$HelpTransportFuture = factor(x.dataset$HelpTransportFuture,
                                      levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                      labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpTransportFuture <- as.numeric(as.character(x.dataset$HelpTransportFuture))
  
  # in Help with Medicine
  x.dataset$HelpMedFuture[is.na(x.dataset$HelpMedFuture)] = "-1"
  x.dataset$HelpMedFuture = factor(x.dataset$HelpMedFuture,
                                levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpMedFuture <- as.numeric(as.character(x.dataset$HelpMedFuture))
  
  # in Help with Finance
  x.dataset$HelpFinanceFuture[is.na(x.dataset$HelpFinanceFuture)] = "-1"
  x.dataset$HelpFinanceFuture = factor(x.dataset$HelpFinanceFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpFinanceFuture <- as.numeric(as.character(x.dataset$HelpFinanceFuture))
  
  # in Help with Digital Service
  x.dataset$HelpDigitalFuture[is.na(x.dataset$HelpDigitalFuture)] = "-1"
  x.dataset$HelpDigitalFuture = factor(x.dataset$HelpDigitalFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
  x.dataset$HelpDigitalFuture <- as.numeric(as.character(x.dataset$HelpDigitalFuture))
  
  # Factorize ADL -------------------------------------------------
  # in ADL Bathing
  x.dataset$ADLBath = factor(x.dataset$ADLBath,
                          levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                          labels = c("0","1","2","3","4"))
  x.dataset$ADLBath <- as.numeric(as.character(x.dataset$ADLBath))
  
  # in ADL Grooming
  x.dataset$ADLGroom = factor(x.dataset$ADLGroom,
                           levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                           labels = c("0","1","2","3","4"))
  x.dataset$ADLGroom <- as.numeric(as.character(x.dataset$ADLGroom))
  
  # in ADL Toileting
  x.dataset$ADLToilet = factor(x.dataset$ADLToilet,
                            levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                            labels = c("0","1","2","3","4"))
  x.dataset$ADLToilet <- as.numeric(as.character(x.dataset$ADLToilet))
  
  # in ADL Transferring
  x.dataset$ADLTransfer = factor(x.dataset$ADLTransfer,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
  x.dataset$ADLTransfer <- as.numeric(as.character(x.dataset$ADLTransfer))
  
  # in ADL Incontinence
  x.dataset$ADLIncontinence = factor(x.dataset$ADLIncontinence,
                                  levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                  labels = c("0","1","2","3","4"))
  x.dataset$ADLIncontinence <- as.numeric(as.character(x.dataset$ADLIncontinence))
  
  # in ADL Feeding
  x.dataset$ADLFeed = factor(x.dataset$ADLFeed,
                          levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                          labels = c("0","1","2","3","4"))
  x.dataset$ADLFeed <- as.numeric(as.character(x.dataset$ADLFeed))
  # Factorize Financial Task -------------------------------------------------
  # in Help with Decision Support
  x.dataset$FinanceDecisionSupport = factor(x.dataset$FinanceDecisionSupport,
                                         levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                         labels = c("0","1","2","3","4"))
  x.dataset$FinanceDecisionSupport <- as.numeric(as.character(x.dataset$FinanceDecisionSupport))
  
  # in Help with Finance Action
  x.dataset$FinanceAction = factor(x.dataset$FinanceAction,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
  x.dataset$FinanceAction <- as.numeric(as.character(x.dataset$FinanceAction))
  
  # in Help with Finance LPA
  x.dataset$FinanceLPA = factor(x.dataset$FinanceLPA,
                             levels = c("No","Yes"),
                             labels = c("0","1"))
  x.dataset$FinanceLPA <- as.numeric(as.character(x.dataset$FinanceLPA))
  
  # in Help with Finance LPA Personal Use
  x.dataset$FinanceLPAUsePersonal[is.na(x.dataset$FinanceLPAUsePersonal)] = "-1"
  x.dataset$FinanceLPAUsePersonal = factor(x.dataset$FinanceLPAUsePersonal,
                                        levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),
                                        labels = c("-1","0","1","2","3","4"))
  x.dataset$FinanceLPAUsePersonal <- as.numeric(as.character(x.dataset$FinanceLPAUsePersonal))
  
  # in Help with Finance LPA Property Use
  x.dataset$FinanceLPAUseProperty[is.na(x.dataset$FinanceLPAUseProperty)] = "-1"
  x.dataset$FinanceLPAUseProperty = factor(x.dataset$FinanceLPAUseProperty,
                                        levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),
                                        labels = c("-1","0","1","2","3","4"))
  x.dataset$FinanceLPAUseProperty <- as.numeric(as.character(x.dataset$FinanceLPAUseProperty))
  
  # Factorize Financial Task (Future)-------------------------------------------------
  # in Help with Decision Support
  x.dataset$FinanceDecisionSupportFuture[is.na(x.dataset$FinanceDecisionSupportFuture)] = "-1"
  x.dataset$FinanceDecisionSupportFuture = factor(x.dataset$FinanceDecisionSupportFuture,
                                               levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                               labels = c("-1","0","1","2","3","4"))
  x.dataset$FinanceDecisionSupportFuture <- as.numeric(as.character(x.dataset$FinanceDecisionSupportFuture))
  
  # in Help with Finance Action
  x.dataset$FinanceActionFuture[is.na(x.dataset$FinanceActionFuture)] = "-1"
  x.dataset$FinanceActionFuture = factor(x.dataset$FinanceActionFuture,
                                      levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                      labels = c("-1","0","1","2","3","4"))
  x.dataset$FinanceActionFuture <- as.numeric(as.character(x.dataset$FinanceActionFuture))
  
  # Factorize Medical Task Low Stake -------------------------------------------------
  # in Help with Making Medical Appt
  x.dataset$MedicalMakeAppt = factor(x.dataset$MedicalMakeAppt,
                                  levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                  labels = c("0","1","2","3","4"))
  x.dataset$MedicalMakeAppt <- as.numeric(as.character(x.dataset$MedicalMakeAppt))
  
  # in Accompanying with Medical Appt
  x.dataset$MedicalAccompanyAppt = factor(x.dataset$MedicalAccompanyAppt,
                                       levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                       labels = c("0","1","2","3","4"))
  x.dataset$MedicalAccompanyAppt <- as.numeric(as.character(x.dataset$MedicalAccompanyAppt))
  
  # in Help with Taking Medicine
  x.dataset$MedicalTakeMeds = factor(x.dataset$MedicalTakeMeds,
                                  levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                  labels = c("0","1","2","3","4"))
  x.dataset$MedicalTakeMeds <- as.numeric(as.character(x.dataset$MedicalTakeMeds))
  
  # in Help with Overseeing Treatment
  x.dataset$MedicalSeeTreatment = factor(x.dataset$MedicalSeeTreatment,
                                      levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                      labels = c("0","1","2","3","4"))
  x.dataset$MedicalSeeTreatment <- as.numeric(as.character(x.dataset$MedicalSeeTreatment))
  
  # Factorize Medical Task Low Stake (future) -------------------------------------------------
  x.dataset$MedicalMakeApptFuture[is.na(x.dataset$MedicalMakeApptFuture)] = "-1"
  x.dataset$MedicalMakeApptFuture = factor(x.dataset$MedicalMakeApptFuture,
                                        levels = c("-1","V.lnlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                        labels = c("-1","0","1","2","3","4"))
  x.dataset$MedicalMakeApptFuture <- as.numeric(as.character(x.dataset$MedicalMakeApptFuture))
  
  x.dataset$MedicalAccompanyApptFuture[is.na(x.dataset$MedicalAccompanyApptFuture)] = "-1"
  x.dataset$MedicalAccompanyApptFuture = factor(x.dataset$MedicalAccompanyApptFuture,
                                             levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                             labels = c("-1","0","1","2","3","4"))
  x.dataset$MedicalAccompanyApptFuture <- as.numeric(as.character(x.dataset$MedicalAccompanyApptFuture))
  
  x.dataset$MedicalTakeMedsFuture[is.na(x.dataset$MedicalTakeMedsFuture)] = "-1"
  x.dataset$MedicalTakeMedsFuture = factor(x.dataset$MedicalTakeMedsFuture,
                                        levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                        labels = c("-1","0","1","2","3","4"))
  x.dataset$MedicalTakeMedsFuture <- as.numeric(as.character(x.dataset$MedicalTakeMedsFuture))
  
  x.dataset$MedicalSeeTreatmentFuture[is.na(x.dataset$MedicalSeeTreatmentFuture)] = "-1"
  x.dataset$MedicalSeeTreatmentFuture = factor(x.dataset$MedicalSeeTreatmentFuture,
                                            levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                            labels = c("-1","0","1","2","3","4"))
  x.dataset$MedicalSeeTreatmentFuture <- as.numeric(as.character(x.dataset$MedicalSeeTreatmentFuture))
  
  # Factorize Medical Task High Stake -------------------------------------------------
  # Making medical decisions
  x.dataset$MedicalDecisionPerson = factor(x.dataset$MedicalDecisionPerson,
                                        levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                        labels = c("0","1","2"))
  x.dataset$MedicalDecisionPerson <- as.numeric(as.character(x.dataset$MedicalDecisionPerson))
  
  x.dataset$MedicalDecisionDoctor = factor(x.dataset$MedicalDecisionDoctor,
                                        levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                        labels = c("0","1","2"))
  x.dataset$MedicalDecisionDoctor <- as.numeric(as.character(x.dataset$MedicalDecisionDoctor))
  
  x.dataset$MedicalSignPapers = factor(x.dataset$MedicalSignPapers,
                                    levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                    labels = c("0","1","2"))
  x.dataset$MedicalSignPapers <- as.numeric(as.character(x.dataset$MedicalSignPapers))
  
  x.dataset$MedicalDecision = rowSums(x.dataset[ , c("MedicalDecisionPerson","MedicalDecisionDoctor","MedicalSignPapers")]) 
  
  # Quality of life
  x.dataset$MedicalWorthPerson = factor(x.dataset$MedicalWorthPerson,
                                     levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                     labels = c("0","1","2"))
  x.dataset$MedicalWorthPerson <- as.numeric(as.character(x.dataset$MedicalWorthPerson))
  
  x.dataset$MedicalWorthDoctor = factor(x.dataset$MedicalWorthDoctor,
                                     levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                     labels = c("0","1","2"))
  x.dataset$MedicalWorthDoctor <- as.numeric(as.character(x.dataset$MedicalWorthDoctor))
  
  x.dataset$MedicalWorthFamily = factor(x.dataset$MedicalWorthFamily,
                                     levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                     labels = c("0","1","2"))
  x.dataset$MedicalWorthFamily <- as.numeric(as.character(x.dataset$MedicalWorthFamily))
  
  x.dataset$MedicalWorth = rowSums(x.dataset[ , c("MedicalWorthPerson","MedicalWorthDoctor","MedicalWorthFamily")]) 
  
  # Medical Care - end of life
  x.dataset$MedicalEndPerson = factor(x.dataset$MedicalEndPerson,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
  x.dataset$MedicalEndPerson <- as.numeric(as.character(x.dataset$MedicalEndPerson))
  
  x.dataset$MedicalEndDoctor = factor(x.dataset$MedicalEndDoctor,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
  x.dataset$MedicalEndDoctor <- as.numeric(as.character(x.dataset$MedicalEndDoctor))
  
  x.dataset$MedicalEndFamily = factor(x.dataset$MedicalEndFamily,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
  x.dataset$MedicalEndFamily <- as.numeric(as.character(x.dataset$MedicalEndFamily))
  
  x.dataset$MedicalEnd = rowSums(x.dataset[ , c("MedicalEndPerson","MedicalEndDoctor","MedicalEndFamily")]) 
  
  # Flexibility for Surrogate in Making Decisions
  x.dataset$MedicalFlexibilityPerson = factor(x.dataset$MedicalFlexibilityPerson,
                                           levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                           labels = c("0","1","2"))
  x.dataset$MedicalFlexibilityPerson <- as.numeric(as.character(x.dataset$MedicalFlexibilityPerson))
  
  x.dataset$MedicalFlexibilityDoctor = factor(x.dataset$MedicalFlexibilityDoctor,
                                           levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                           labels = c("0","1","2"))
  x.dataset$MedicalFlexibilityDoctor <- as.numeric(as.character(x.dataset$MedicalFlexibilityDoctor))
  
  x.dataset$MedicalFlexibilityFamily = factor(x.dataset$MedicalFlexibilityFamily,
                                           levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                           labels = c("0","1","2"))
  x.dataset$MedicalFlexibilityFamily <- as.numeric(as.character(x.dataset$MedicalFlexibilityFamily))
  
  x.dataset$MedicalDoctorQuestions = factor(x.dataset$MedicalDoctorQuestions,
                                         levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                         labels = c("0","1","2"))
  x.dataset$MedicalDoctorQuestions <- as.numeric(as.character(x.dataset$MedicalDoctorQuestions))
  
  x.dataset$MedicalSurrogate = rowSums(x.dataset[ , c("MedicalFlexibilityPerson","MedicalFlexibilityDoctor","MedicalFlexibilityFamily","MedicalDoctorQuestions")]) 
  
  x.dataset$MedicalHighStake = rowSums(x.dataset[ , c("MedicalDecision","MedicalWorth","MedicalEnd","MedicalSurrogate")]) 
  x.dataset$MedicalHighStake <- scale_values(x.dataset$MedicalHighStake)
  x.dataset$MedicalHighStake <- round(x.dataset$MedicalHighStake ,digit=2)
  
  # in Managing with Caregiver Account (Num)
  unique(x.dataset$DigitalCaregiverAccountNum)
  x.dataset$DigitalCaregiverAccountNum = factor(x.dataset$DigitalCaregiverAccountNum,
                                             levels = c("0","1","2","3","4","5","8","10","12","17","23",">30",">=100"),
                                             labels = c("0","1","2","3","4","5","8","10","12","17","23","31","100"))
  x.dataset$DigitalCaregiverAccountNum <- as.numeric(as.character(x.dataset$DigitalCaregiverAccountNum))
  
  return(x.dataset)
}

# This function factor digital delegations
DigitalDelegation <- function(x.dataset)
{
  # GROUPING DIGITAL DELEGATE HERE >> IMPORTANT-------------------------------------------------
  #Q5.15: I have a joint account (note: the raw values can be retrieved via 'FinanceJointAccount_raw' variable)
  x.dataset$FinanceJointAccount = factor(x.dataset$FinanceJointAccount,
                                         levels = c("No","Yes","Unsure"),
                                         labels = c("0","1","0"))
  x.dataset$FinanceJointAccount <- as.numeric(as.character(x.dataset$FinanceJointAccount))
  
  #Q5.17: I have helped with digital financial access
  x.dataset$FinanceDigitalAssistance = factor(x.dataset$FinanceDigitalAssistance,
                                              levels = c("No","Yes"),
                                              labels = c("0","1"))
  x.dataset$FinanceDigitalAssistance <- as.numeric(as.character(x.dataset$FinanceDigitalAssistance))
  
  #Q5.19: Own a separate account (note: the raw values can be retrieved via 'FinanceLoginWays' variable)
  x.dataset$FinanceLoginInstitution[is.na(x.dataset$FinanceLoginWays_institution)] = "2"
  x.dataset$FinanceLoginInstitution = factor(x.dataset$FinanceLoginWays_institution,
                                             levels = c("No","Yes"),
                                             labels = c("0","1"))
  x.dataset$FinanceLoginInstitution <- as.numeric(as.character(x.dataset$FinanceLoginInstitution))
  
  #Q6.33: I have a joint account
  x.dataset$DigitalCaregiverAccount = factor(x.dataset$DigitalCaregiverAccount,
                                             levels = c("Unsure","No, but I should have","Yes"),
                                             labels = c("0","0","1"))
  x.dataset$DigitalCaregiverAccount <- as.numeric(as.character(x.dataset$DigitalCaregiverAccount))
  
  #Q6.36: I have assisted another adult with digital medical services
  x.dataset$DigitalAssistMedical = factor(x.dataset$DigitalAssistMedical,
                                          levels = c("No",	"Yes"),
                                          labels = c("0",	"1"))
  x.dataset$DigitalAssistMedical <- as.numeric(as.character(x.dataset$DigitalAssistMedical))
  
  #Q6.38: My own separate account(note: the raw values can be retrieved via 'DigitalAssistAccessWays' variable)
  x.dataset$MedicalLoginInstitution[is.na(x.dataset$DigitalAssistAccessWays_institution)] = "2"
  x.dataset$MedicalLoginInstitution = factor(x.dataset$DigitalAssistAccessWays_institution,
                                             levels = c("No","Yes"),
                                             labels = c("0","1"))
  x.dataset$MedicalLoginInstitution <- as.numeric(as.character(x.dataset$MedicalLoginInstitution))
  return(x.dataset)
}

# This function use subset to clean data
filterData <- function(x.dataset)
{
  x.dat <- x.dataset
  x.dat <- subset(x.dat, !(HelpFriends=="101"))
  x.dat <- subset(x.dat, !(FinanceHelpFriends=="101"))
  x.dat <- subset(x.dat, !(MedicalHelpImmediateFam=="101"))
  x.dat <- subset(x.dat, !(MedicalHelpFriends=="101"))
  
  x.dat <- subset(x.dat, !(HelpFriends=="31"))
  x.dat <- subset(x.dat, !(FinanceHelpFriends=="31"))
  x.dat <- subset(x.dat, !(MedicalHelpImmediateFam=="31"))
  x.dat <- subset(x.dat, !(MedicalHelpFriends=="31"))
  x.dat <- subset(x.dat, !(FinanceJointAccount=="Exclude"))
  x.dat <- subset(x.dat, !(Marital=="3"))
  return(x.dat)
}

# Variable including IADL, ADL, low-stake medical activities, high-stake medical activities
testData <- function(x.dataset)
{
  filter.Data <- filterData(x.dataset)
  x.dataset <- filter.Data[,c('Age','Income','Housing','Gender','Education','Employment','Ethnicity','Siblings','Marital')]
  
  #how much help participants have provided for an adult in carrying out their instrumental activities of daily living
  x.dataset$IADL <- rowSums(filter.Data[ , c("HelpTransport","HelpMed","HelpFinance","HelpFood","HelpLaundry","HelpHousekeep")])
  x.dataset$IADL <- scale_values(x.dataset$IADL)
  x.dataset$IADL <- round(x.dataset$IADL ,digit=2)
  
  #how much help participants have provided for an adult in carrying out their activities of daily living
  x.dataset$ADL <- rowSums(filter.Data[ , c("ADLBath","ADLFeed","ADLGroom","ADLIncontinence","ADLToilet","ADLTransfer")]) 
  x.dataset$ADL <- scale_values(x.dataset$ADL)
  x.dataset$ADL <- round(x.dataset$ADL ,digit=2)
  
  #how much help participants have provided for an adult in carrying out low-stake medical care activities
  x.dataset$lowstakeMed <- rowSums(filter.Data[ , c("MedicalMakeAppt","MedicalAccompanyAppt","MedicalTakeMeds","MedicalSeeTreatment")])
  
  #how much help participants have provided for an adult in carrying out high-stake medical care activities
  x.dataset$highstakeMed <- rowSums(filter.Data[ , c("MedicalDecisionPerson","MedicalDecisionDoctor","MedicalSignPapers","MedicalWorthPerson",
                                                    "MedicalWorthDoctor","MedicalWorthFamily","MedicalEndPerson","MedicalEndFamily",
                                                    "MedicalFlexibilityPerson","MedicalFlexibilityDoctor","MedicalFlexibilityFamily",
                                                    "MedicalDoctorQuestions")])
  
  #DEPENDENT VARIABLES HERE
  x.dataset$FinanceDelegate1 <- as.factor(filter.Data$FinanceDelegate1)
  x.dataset$FinanceDelegate2 <- as.factor(filter.Data$FinanceDelegate2)
  x.dataset$MedicalDelegate1 <- as.factor(filter.Data$MedicalDelegate1)
  x.dataset$MedicalDelegate2 <- as.factor(filter.Data$MedicalDelegate2)
  return(x.dataset)
}

################---------------------------------------------------
# DATA MANIPULATION ONLY
################---------------------------------------------------
my.dat.SG <- factorDemographics(my.dat,"SG",FALSE)
my.dat.SG <- factorHelp(my.dat.SG)
my.dat.SG <- DigitalDelegation(my.dat.SG)
sg_filtered_data <- filterData(my.dat.SG)
test.data.sg <- testData(sg_filtered_data)

my.dat.SG.plot <- factorDemographics(my.dat,"SG",TRUE)
my.dat.SG.plot <- factorHelp(my.dat.SG.plot)
my.dat.SG.plot <- DigitalDelegation(my.dat.SG.plot)
sg_filtered_data_plot <- filterData(my.dat.SG.plot)
test.data.sg.plot <- testData(sg_filtered_data_plot)

my.dat.US <- factorDemographics(my.dat,"US",FALSE)
my.dat.US <- factorHelp(my.dat.US)
my.dat.US <- DigitalDelegation(my.dat.US)
us_filtered_data <- filterData(my.dat.US)
test.data.us <- testData(us_filtered_data)

my.dat.US.plot <- factorDemographics(my.dat,"US",TRUE)
my.dat.US.plot <- factorHelp(my.dat.US.plot)
my.dat.US.plot <- DigitalDelegation(my.dat.US.plot)
us_filtered_data_plot <- filterData(my.dat.US.plot)
test.data.us.plot <- testData(us_filtered_data_plot)

# write to file
path_to_r_script <- "~/Desktop/R script submit/test.data.sg.csv"
write.csv(test.data.sg, file = path_to_r_script, row.names = FALSE)

path_to_r_script <- "~/Desktop/R script submit/sg_filtered_data.csv"
write.csv(sg_filtered_data, file = path_to_r_script, row.names = FALSE)

path_to_r_script <- "~/Desktop/R script submit/test.data.sg.plot.csv"
write.csv(test.data.sg.plot, file = path_to_r_script, row.names = FALSE)

path_to_r_script <- "~/Desktop/R script submit/sg_filtered_data_plot.csv"
write.csv(sg_filtered_data_plot, file = path_to_r_script, row.names = FALSE)













