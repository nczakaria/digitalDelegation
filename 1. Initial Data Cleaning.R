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

# Factorize Demographics -------------------------------------------------
my.dat$Country <- factor(my.dat$Country, levels = c("US","SG"), labels = c("US","SG"))

my.dat$Marital[is.na(my.dat$Marital)] = "3"
my.dat$Marital <- factor(my.dat$Marital,
                         levels = c("Unmarried","Married","Div/Sep","Widowed"),
                         labels = c("Unmarried","Married","Once Married","Once Married"))
my.dat$Marital <- relevel(my.dat$Marital, ref = "Once Married")

ordering <- c("21","25", "30", "35", "40", "45", "50", "55", "60", "65", "70")
orderFactor <- factor(my.dat$Age, levels = ordering)
my.dat$Age <- as.numeric(orderFactor)

my.dat$Housing <- factor(my.dat$Housing, 
                         levels = c("low-income housing", "apartment", "familyhouse"),
                         labels = c("0", "1", "2"), ordered = TRUE)
my.dat$Housing <- as.factor(as.character(my.dat$Housing))
my.dat$Housing <- as.numeric(my.dat$Housing) ###

ordering <- c("<$30k", "$30k", "$50k", "$100k", "$150k", ">$200k")
orderFactor <- factor(my.dat$Income, levels = ordering)
my.dat$Income <- as.numeric(orderFactor)

my.dat$Gender <- as.factor(my.dat$Gender)
my.dat$Gender <- as.numeric(my.dat$Gender) ###

my.dat$Ethnicity <- factor(my.dat$Ethnicity, 
                           levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"),
                           labels = c("Majority", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Underrepresented", "Majority", "Underrepresented", "Underrepresented", "Underrepresented"))
my.dat$Ethnicity <- as.factor(as.character(my.dat$Ethnicity))
my.dat$Ethnicity <- as.numeric(my.dat$Ethnicity) ###
# my.dat$Ethnicity <- factor(my.dat$Ethnicity, levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"), labels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", "Chinese", "Malay", "Indian", "Eurasian"))
# my.dat$Ethnicity <- as.factor(as.character(my.dat$Ethnicity))
# table(my.dat$Ethnicity)
# table(test.data.us$Ethnicity)
# table(test.data.sg$Ethnicity)

# religion
filterCountryData <- function(xyz) { # create a function with the name my_function
  test<-my.dat%>%
    filter(Country==xyz)%>%
    return(test)
}
#digital_proxyUS <- filterCountryData("US")
#unique(digital_proxyUS$Religion)
#digital_proxySG <- filterCountryData("SG")
#unique(digital_proxySG$Religion)

my.dat$Religion <- factor(my.dat$Religion, 
                          levels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic","Buddhism","Catholicism","No Religion","Islam","Christianity","Taoism","Hinduism"),
                          labels = c("Christian", "No Religion", "Catholic", "Jehovah Witness", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", "Hindu", "Buddhist", "Agnostic","Buddhism","Catholicism","No Religion","Islam","Christianity","Taoism","Hinduism"))
my.dat$Religion <- as.factor(as.character(my.dat$Religion))

my.dat$Education <- factor(my.dat$Education, 
                           levels = c("Below", "High", "Degree and above"),
                           labels = c("< High School", "High School", "Degree and above"), ordered = TRUE)
ordering <- c("< High School", "High School", "Degree and above")
orderFactor <- factor(my.dat$Education, levels = ordering)
my.dat$Education <- as.numeric(orderFactor)

my.dat$Employment <- factor(my.dat$Employment, 
                            levels = c("Disabled","Retired","Student","Unemployed","Full-time","Part-time","Self"),
                            labels = c("Unemployed","Unemployed","Unemployed","Unemployed","Employed","Employed","Employed"), ordered = TRUE)
my.dat$Employment <- as.factor(as.character(my.dat$Employment))
my.dat$Employment <- as.numeric(my.dat$Employment) ###

ordering <- c("0","1","2","3",">4")
orderFactor <- factor(my.dat$Siblings, levels = ordering)
my.dat$Siblings <- as.numeric(orderFactor)

ordering <- c("Youngest","Middle","Oldest")
orderFactor <- factor(my.dat$SiblingsOrder, levels = ordering)
my.dat$SiblingsOrder <- as.numeric(orderFactor)

# Factorize Physical Help -------------------------------------------------
my.dat$HelpImmediateFam = factor(my.dat$HelpImmediateFam, 
                                 levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25",">30"), 
                                 labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","15","17","20","25","31"))
my.dat$HelpImmediateFam <- as.numeric(as.character(my.dat$HelpImmediateFam))

# in relatives
my.dat$HelpRelatives = factor(my.dat$HelpRelatives, 
                              levels = c("0","1","2","3","4","5","6","9","10","16","20","29",">30"), 
                              labels = c("0","1","2","3","4","5","6","9","10","16","20","29","31"))
my.dat$HelpRelatives <- as.numeric(as.character(my.dat$HelpRelatives))

# in Friends
my.dat$HelpFriends = factor(my.dat$HelpFriends, 
                            levels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30",">30",">=100"), 
                            labels = c("0","1","2","3","4","5","6","7","8","10","11","12","15","20","25","30","31","101"))
my.dat$HelpFriends <- as.numeric(as.character(my.dat$HelpFriends))

# in Pass Away
my.dat$HelpPassAway[is.na(my.dat$HelpPassAway)] = -1
my.dat$HelpPassAway = factor(my.dat$HelpPassAway, 
                             levels = c("-1","0","1","2","3",">3"), 
                             labels = c("-1","0","1","2","3","4"))
my.dat$HelpPassAway <- as.numeric(as.character(my.dat$HelpPassAway))

# Factorize Financial Help -------------------------------------------------
my.dat$FinanceHelpImmediateFam = factor(my.dat$FinanceHelpImmediateFam, 
                                        levels = c("0","1","2","3","4","5","6","10","12","18","20",">30"), 
                                        labels = c("0","1","2","3","4","5","6","10","12","18","20","31"))
my.dat$FinanceHelpImmediateFam <- as.numeric(as.character(my.dat$FinanceHelpImmediateFam))

# in relatives
my.dat$FinanceHelpRelatives = factor(my.dat$FinanceHelpRelatives, 
                                     levels = c("0","1","2","3","5","6","7","8","10","20","30",">30"), 
                                     labels = c("0","1","2","3","5","6","7","8","10","20","30","31"))
my.dat$FinanceHelpRelatives <- as.numeric(as.character(my.dat$FinanceHelpRelatives))

# in Friends
my.dat$FinanceHelpFriends = factor(my.dat$FinanceHelpFriends, 
                                   levels = c("0","1","2","3","4","5","6","7","9","10","12","15","20",">30",">100"), 
                                   labels = c("0","1","2","3","4","5","6","7","9","10","12","15","20","31","101"))
my.dat$FinanceHelpFriends <- as.numeric(as.character(my.dat$FinanceHelpFriends))


# in Pass Away
my.dat$FinanceHelpPassAway[is.na(my.dat$FinanceHelpPassAway)] = -1
my.dat$FinanceHelpPassAway = factor(my.dat$FinanceHelpPassAway, 
                                    levels = c("-1","0","1","2","3",">3"), 
                                    labels = c("-1","0","1","2","3","4"))
my.dat$FinanceHelpPassAway <- as.numeric(as.character(my.dat$FinanceHelpPassAway))

# Factorize Medical Help -------------------------------------------------
my.dat$MedicalHelpImmediateFam = factor(my.dat$MedicalHelpImmediateFam, 
                                        levels = c("0","1","2","3","4","5","6","10","12","18","20",">30",">100"), 
                                        labels = c("0","1","2","3","4","5","6","10","12","18","20","31","101"))
my.dat$MedicalHelpImmediateFam <- as.numeric(as.character(my.dat$MedicalHelpImmediateFam))

# in relatives
my.dat$MedicalHelpRelatives = factor(my.dat$MedicalHelpRelatives, 
                                     levels = c("0","1","2","3","4","5","6","7","10","20",">30"), 
                                     labels = c("0","1","2","3","4","5","6","7","10","20","31"))
my.dat$MedicalHelpRelatives <- as.numeric(as.character(my.dat$MedicalHelpRelatives))

# in Friends
my.dat$MedicalHelpFriends = factor(my.dat$MedicalHelpFriends, 
                                   levels = c("0","1","2","3","4","5","6","7","10","15","20","26",">30",">=100"), 
                                   labels = c("0","1","2","3","4","5","6","7","10","15","20","26","31","101"))
my.dat$MedicalHelpFriends <- as.numeric(as.character(my.dat$MedicalHelpFriends))

# in Pass Away
my.dat$MedicalHelpPassAway[is.na(my.dat$MedicalHelpPassAway)] = -1
my.dat$MedicalHelpPassAway = factor(my.dat$MedicalHelpPassAway, 
                                    levels = c("-1","0","1","2","3",">3"), 
                                    labels = c("-1","0","1","2","3","4"))
my.dat$MedicalHelpPassAway <- as.numeric(as.character(my.dat$MedicalHelpPassAway))

# Factorize IADL -------------------------------------------------
# in Help with Food
my.dat$HelpFood = factor(my.dat$HelpFood,
                         levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                         labels = c("0","1","2","3","4"))
my.dat$HelpFood <- as.numeric(as.character(my.dat$HelpFood))

# in Help with Housekeeping
my.dat$HelpHousekeep = factor(my.dat$HelpHousekeep,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
my.dat$HelpHousekeep <- as.numeric(as.character(my.dat$HelpHousekeep))

# in Help with Laundry
my.dat$HelpLaundry = factor(my.dat$HelpLaundry,
                            levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                            labels = c("0","1","2","3","4"))
my.dat$HelpLaundry <- as.numeric(as.character(my.dat$HelpLaundry))

# in Help with Transport
my.dat$HelpTransport = factor(my.dat$HelpTransport,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
my.dat$HelpTransport <- as.numeric(as.character(my.dat$HelpTransport))

# in Help with Medicine
my.dat$HelpMed = factor(my.dat$HelpMed,
                        levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                        labels = c("0","1","2","3","4"))
my.dat$HelpMed <- as.numeric(as.character(my.dat$HelpMed))

# in Help with Finance
my.dat$HelpFinance = factor(my.dat$HelpFinance,
                            levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                            labels = c("0","1","2","3","4"))
my.dat$HelpFinance <- as.numeric(as.character(my.dat$HelpFinance))

# in Help with Digital Service
my.dat$HelpDigital = factor(my.dat$HelpDigital,
                            levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                            labels = c("0","1","2","3","4"))
my.dat$HelpDigital <- as.numeric(as.character(my.dat$HelpDigital))

# Factorize IADL(future) -------------------------------------------------
# in Help with Food
my.dat$HelpFoodFuture[is.na(my.dat$HelpFoodFuture)] = "-1"
my.dat$HelpFoodFuture = factor(my.dat$HelpFoodFuture,
                               levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                               labels = c("-1","0","1","2","3","4"))
my.dat$HelpFoodFuture <- as.numeric(as.character(my.dat$HelpFoodFuture))

# in Help with Housekeeping
my.dat$HelpHousekeepFuture[is.na(my.dat$HelpHousekeepFuture)] = "-1"
my.dat$HelpHousekeepFuture = factor(my.dat$HelpHousekeepFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
my.dat$HelpHousekeepFuture <- as.numeric(as.character(my.dat$HelpHousekeepFuture))

# in Help with Laundry
my.dat$HelpLaundryFuture[is.na(my.dat$HelpLaundryFuture)] = "-1"
my.dat$HelpLaundryFuture = factor(my.dat$HelpLaundryFuture,
                                  levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                  labels = c("-1","0","1","2","3","4"))
my.dat$HelpLaundryFuture <- as.numeric(as.character(my.dat$HelpLaundryFuture))

# in Help with Transport
my.dat$HelpTransportFuture[is.na(my.dat$HelpTransportFuture)] = "-1"
my.dat$HelpTransportFuture = factor(my.dat$HelpTransportFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
my.dat$HelpTransportFuture <- as.numeric(as.character(my.dat$HelpTransportFuture))

# in Help with Medicine
my.dat$HelpMedFuture[is.na(my.dat$HelpMedFuture)] = "-1"
my.dat$HelpMedFuture = factor(my.dat$HelpMedFuture,
                              levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                              labels = c("-1","0","1","2","3","4"))
my.dat$HelpMedFuture <- as.numeric(as.character(my.dat$HelpMedFuture))

# in Help with Finance
my.dat$HelpFinanceFuture[is.na(my.dat$HelpFinanceFuture)] = "-1"
my.dat$HelpFinanceFuture = factor(my.dat$HelpFinanceFuture,
                                  levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                  labels = c("-1","0","1","2","3","4"))
my.dat$HelpFinanceFuture <- as.numeric(as.character(my.dat$HelpFinanceFuture))

# in Help with Digital Service
my.dat$HelpDigitalFuture[is.na(my.dat$HelpDigitalFuture)] = "-1"
my.dat$HelpDigitalFuture = factor(my.dat$HelpDigitalFuture,
                                  levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                  labels = c("-1","0","1","2","3","4"))
my.dat$HelpDigitalFuture <- as.numeric(as.character(my.dat$HelpDigitalFuture))

# Factorize ADL -------------------------------------------------
# in ADL Bathing
my.dat$ADLBath = factor(my.dat$ADLBath,
                        levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                        labels = c("0","1","2","3","4"))
my.dat$ADLBath <- as.numeric(as.character(my.dat$ADLBath))

# in ADL Grooming
my.dat$ADLGroom = factor(my.dat$ADLGroom,
                         levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                         labels = c("0","1","2","3","4"))
my.dat$ADLGroom <- as.numeric(as.character(my.dat$ADLGroom))

# in ADL Toileting
my.dat$ADLToilet = factor(my.dat$ADLToilet,
                          levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                          labels = c("0","1","2","3","4"))
my.dat$ADLToilet <- as.numeric(as.character(my.dat$ADLToilet))

# in ADL Transferring
my.dat$ADLTransfer = factor(my.dat$ADLTransfer,
                            levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                            labels = c("0","1","2","3","4"))
my.dat$ADLTransfer <- as.numeric(as.character(my.dat$ADLTransfer))

# in ADL Incontinence
my.dat$ADLIncontinence = factor(my.dat$ADLIncontinence,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
my.dat$ADLIncontinence <- as.numeric(as.character(my.dat$ADLIncontinence))

# in ADL Feeding
my.dat$ADLFeed = factor(my.dat$ADLFeed,
                        levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                        labels = c("0","1","2","3","4"))
my.dat$ADLFeed <- as.numeric(as.character(my.dat$ADLFeed))

# Factorize Financial Task -------------------------------------------------
# in Help with Decision Support
my.dat$FinanceDecisionSupport = factor(my.dat$FinanceDecisionSupport,
                                       levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                       labels = c("0","1","2","3","4"))
my.dat$FinanceDecisionSupport <- as.numeric(as.character(my.dat$FinanceDecisionSupport))

# in Help with Finance Action
my.dat$FinanceAction = factor(my.dat$FinanceAction,
                              levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                              labels = c("0","1","2","3","4"))
my.dat$FinanceAction <- as.numeric(as.character(my.dat$FinanceAction))

# in Help with Finance LPA
my.dat$FinanceLPA = factor(my.dat$FinanceLPA,
                           levels = c("No","Yes"),
                           labels = c("0","1"))
my.dat$FinanceLPA <- as.numeric(as.character(my.dat$FinanceLPA))

# in Help with Finance LPA Personal Use
my.dat$FinanceLPAUsePersonal[is.na(my.dat$FinanceLPAUsePersonal)] = "-1"
my.dat$FinanceLPAUsePersonal = factor(my.dat$FinanceLPAUsePersonal,
                                      levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),
                                      labels = c("-1","0","1","2","3","4"))
my.dat$FinanceLPAUsePersonal <- as.numeric(as.character(my.dat$FinanceLPAUsePersonal))

# in Help with Finance LPA Property Use
my.dat$FinanceLPAUseProperty[is.na(my.dat$FinanceLPAUseProperty)] = "-1"
my.dat$FinanceLPAUseProperty = factor(my.dat$FinanceLPAUseProperty,
                                      levels = c("-1","Never","Rarely","Sometimes","Frequently","Always"),
                                      labels = c("-1","0","1","2","3","4"))
my.dat$FinanceLPAUseProperty <- as.numeric(as.character(my.dat$FinanceLPAUseProperty))

# in Help with Finance LPA Property Use
#my.dat$FinanceLPAYear[is.na(my.dat$FinanceLPAYear)] = "-1"
#my.dat$FinanceLPAYear = factor(my.dat$FinanceLPAYear,
#                                      levels = c("-1","<EPA","'82","'88","'87","'89","'90","'91","'92","'94","'95","'98",
#                                                 "'99","'00","'01","'02","'04","'05","'06","'07","'08","'09","'10",
#                                                 "'11","'12","'13","'14","'15","'16","'17","'18","'19","'20","'21","'22","Future"),
#                                      labels = c("-1","<EPA","'82","'88","'87","'89","'90","'91","'92","'94","'95","'98",
#                                                 "'99","'00","'01","'02","'04","'05","'06","'07","'08","'09","'10",
#                                                 "'11","'12","'13","'14","'15","'16","'17","'18","'19","'20","'21","'22","Future"))
#my.dat$FinanceLPAYear <- as.numeric(as.character(my.dat$FinanceLPAYear))

# Factorize Financial Task (Future)-------------------------------------------------
# in Help with Decision Support
my.dat$FinanceDecisionSupportFuture[is.na(my.dat$FinanceDecisionSupportFuture)] = "-1"
my.dat$FinanceDecisionSupportFuture = factor(my.dat$FinanceDecisionSupportFuture,
                                             levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                             labels = c("-1","0","1","2","3","4"))
my.dat$FinanceDecisionSupportFuture <- as.numeric(as.character(my.dat$FinanceDecisionSupportFuture))

# in Help with Finance Action
my.dat$FinanceActionFuture[is.na(my.dat$FinanceActionFuture)] = "-1"
my.dat$FinanceActionFuture = factor(my.dat$FinanceActionFuture,
                                    levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                    labels = c("-1","0","1","2","3","4"))
my.dat$FinanceActionFuture <- as.numeric(as.character(my.dat$FinanceActionFuture))

# Factorize Medical Task Low Stake -------------------------------------------------
# in Help with Making Medical Appt
my.dat$MedicalMakeAppt = factor(my.dat$MedicalMakeAppt,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
my.dat$MedicalMakeAppt <- as.numeric(as.character(my.dat$MedicalMakeAppt))

# in Accompanying with Medical Appt
my.dat$MedicalAccompanyAppt = factor(my.dat$MedicalAccompanyAppt,
                                     levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                     labels = c("0","1","2","3","4"))
my.dat$MedicalAccompanyAppt <- as.numeric(as.character(my.dat$MedicalAccompanyAppt))

# in Help with Taking Medicine
my.dat$MedicalTakeMeds = factor(my.dat$MedicalTakeMeds,
                                levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                labels = c("0","1","2","3","4"))
my.dat$MedicalTakeMeds <- as.numeric(as.character(my.dat$MedicalTakeMeds))

# in Help with Overseeing Treatment
my.dat$MedicalSeeTreatment = factor(my.dat$MedicalSeeTreatment,
                                    levels = c("Never","Yearly","Monthly","Weekly","Daily"),
                                    labels = c("0","1","2","3","4"))
my.dat$MedicalSeeTreatment <- as.numeric(as.character(my.dat$MedicalSeeTreatment))

# Factorize Medical Task Low Stake (future) -------------------------------------------------
my.dat$MedicalMakeApptFuture[is.na(my.dat$MedicalMakeApptFuture)] = "-1"
my.dat$MedicalMakeApptFuture = factor(my.dat$MedicalMakeApptFuture,
                                      levels = c("-1","V.lnlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                      labels = c("-1","0","1","2","3","4"))
my.dat$MedicalMakeApptFuture <- as.numeric(as.character(my.dat$MedicalMakeApptFuture))

my.dat$MedicalAccompanyApptFuture[is.na(my.dat$MedicalAccompanyApptFuture)] = "-1"
my.dat$MedicalAccompanyApptFuture = factor(my.dat$MedicalAccompanyApptFuture,
                                           levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                           labels = c("-1","0","1","2","3","4"))
my.dat$MedicalAccompanyApptFuture <- as.numeric(as.character(my.dat$MedicalAccompanyApptFuture))

my.dat$MedicalTakeMedsFuture[is.na(my.dat$MedicalTakeMedsFuture)] = "-1"
my.dat$MedicalTakeMedsFuture = factor(my.dat$MedicalTakeMedsFuture,
                                      levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                      labels = c("-1","0","1","2","3","4"))
my.dat$MedicalTakeMedsFuture <- as.numeric(as.character(my.dat$MedicalTakeMedsFuture))

my.dat$MedicalSeeTreatmentFuture[is.na(my.dat$MedicalSeeTreatmentFuture)] = "-1"
my.dat$MedicalSeeTreatmentFuture = factor(my.dat$MedicalSeeTreatmentFuture,
                                          levels = c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely"),
                                          labels = c("-1","0","1","2","3","4"))
my.dat$MedicalSeeTreatmentFuture <- as.numeric(as.character(my.dat$MedicalSeeTreatmentFuture))

# Factorize Medical Task High Stake -------------------------------------------------
# Making medical decisions
my.dat$MedicalDecisionPerson = factor(my.dat$MedicalDecisionPerson,
                                      levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                      labels = c("0","1","2"))
my.dat$MedicalDecisionPerson <- as.numeric(as.character(my.dat$MedicalDecisionPerson))

my.dat$MedicalDecisionDoctor = factor(my.dat$MedicalDecisionDoctor,
                                      levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                      labels = c("0","1","2"))
my.dat$MedicalDecisionDoctor <- as.numeric(as.character(my.dat$MedicalDecisionDoctor))

my.dat$MedicalSignPapers = factor(my.dat$MedicalSignPapers,
                                  levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                  labels = c("0","1","2"))
my.dat$MedicalSignPapers <- as.numeric(as.character(my.dat$MedicalSignPapers))

my.dat$MedicalDecision = rowSums(my.dat[ , c("MedicalDecisionPerson","MedicalDecisionDoctor","MedicalSignPapers")]) 

# Quality of life
my.dat$MedicalWorthPerson = factor(my.dat$MedicalWorthPerson,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
my.dat$MedicalWorthPerson <- as.numeric(as.character(my.dat$MedicalWorthPerson))

my.dat$MedicalWorthDoctor = factor(my.dat$MedicalWorthDoctor,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
my.dat$MedicalWorthDoctor <- as.numeric(as.character(my.dat$MedicalWorthDoctor))

my.dat$MedicalWorthFamily = factor(my.dat$MedicalWorthFamily,
                                   levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                   labels = c("0","1","2"))
my.dat$MedicalWorthFamily <- as.numeric(as.character(my.dat$MedicalWorthFamily))

my.dat$MedicalWorth = rowSums(my.dat[ , c("MedicalWorthPerson","MedicalWorthDoctor","MedicalWorthFamily")]) 

# Medical Care - end of life
my.dat$MedicalEndPerson = factor(my.dat$MedicalEndPerson,
                                 levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                 labels = c("0","1","2"))
my.dat$MedicalEndPerson <- as.numeric(as.character(my.dat$MedicalEndPerson))

my.dat$MedicalEndDoctor = factor(my.dat$MedicalEndDoctor,
                                 levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                 labels = c("0","1","2"))
my.dat$MedicalEndDoctor <- as.numeric(as.character(my.dat$MedicalEndDoctor))

my.dat$MedicalEndFamily = factor(my.dat$MedicalEndFamily,
                                 levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                 labels = c("0","1","2"))
my.dat$MedicalEndFamily <- as.numeric(as.character(my.dat$MedicalEndFamily))

my.dat$MedicalEnd = rowSums(my.dat[ , c("MedicalEndPerson","MedicalEndDoctor","MedicalEndFamily")]) 

# Flexibility for Surrogate in Making Decisions
my.dat$MedicalFlexibilityPerson = factor(my.dat$MedicalFlexibilityPerson,
                                         levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                         labels = c("0","1","2"))
my.dat$MedicalFlexibilityPerson <- as.numeric(as.character(my.dat$MedicalFlexibilityPerson))

my.dat$MedicalFlexibilityDoctor = factor(my.dat$MedicalFlexibilityDoctor,
                                         levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                         labels = c("0","1","2"))
my.dat$MedicalFlexibilityDoctor <- as.numeric(as.character(my.dat$MedicalFlexibilityDoctor))

my.dat$MedicalFlexibilityFamily = factor(my.dat$MedicalFlexibilityFamily,
                                         levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                         labels = c("0","1","2"))
my.dat$MedicalFlexibilityFamily <- as.numeric(as.character(my.dat$MedicalFlexibilityFamily))

my.dat$MedicalDoctorQuestions = factor(my.dat$MedicalDoctorQuestions,
                                       levels = c("No, because I didn't need to","No, but I should have","Yes"),
                                       labels = c("0","1","2"))
my.dat$MedicalDoctorQuestions <- as.numeric(as.character(my.dat$MedicalDoctorQuestions))

my.dat$MedicalSurrogate = rowSums(my.dat[ , c("MedicalFlexibilityPerson","MedicalFlexibilityDoctor","MedicalFlexibilityFamily","MedicalDoctorQuestions")]) 

my.dat$MedicalHighStake = rowSums(my.dat[ , c("MedicalDecision","MedicalWorth","MedicalEnd","MedicalSurrogate")]) 
my.dat$MedicalHighStake <- scale_values(my.dat$MedicalHighStake)
my.dat$MedicalHighStake <- round(my.dat$MedicalHighStake ,digit=2)

# in Managing with Caregiver Account (Num)
unique(my.dat$DigitalCaregiverAccountNum)
my.dat$DigitalCaregiverAccountNum = factor(my.dat$DigitalCaregiverAccountNum,
                                           levels = c("0","1","2","3","4","5","8","10","12","17","23",">30",">=100"),
                                           labels = c("0","1","2","3","4","5","8","10","12","17","23","31","100"))
my.dat$DigitalCaregiverAccountNum <- as.numeric(as.character(my.dat$DigitalCaregiverAccountNum))

# GROUPING DIGITAL DELEGATE HERE >> IMPORTANT-------------------------------------------------
#Q5.15: I have a joint account (note: the raw values can be retrieved via 'FinanceJointAccount_raw' variable)
my.dat$FinanceJointAccount = factor(my.dat$FinanceJointAccount,
                                    levels = c("No","Yes","Unsure"),
                                    labels = c("0","1","0"))
my.dat$FinanceJointAccount <- as.numeric(as.character(my.dat$FinanceJointAccount))

#Q5.17: I have helped with digital financial access
my.dat$FinanceDigitalAssistance = factor(my.dat$FinanceDigitalAssistance,
                                         levels = c("No","Yes"),
                                         labels = c("0","1"))
my.dat$FinanceDigitalAssistance <- as.numeric(as.character(my.dat$FinanceDigitalAssistance))

#Q5.19: Own a separate account (note: the raw values can be retrieved via 'FinanceLoginWays' variable)
my.dat$FinanceLoginInstitution[is.na(my.dat$FinanceLoginWays_institution)] = "2"
my.dat$FinanceLoginInstitution = factor(my.dat$FinanceLoginWays_institution,
                                        levels = c("No","Yes"),
                                        labels = c("0","1"))
my.dat$FinanceLoginInstitution <- as.numeric(as.character(my.dat$FinanceLoginInstitution))

#Q6.33: I have a joint account
my.dat$DigitalCaregiverAccount = factor(my.dat$DigitalCaregiverAccount,
                                        levels = c("Unsure","No, but I should have","Yes"),
                                        labels = c("0","0","1"))
my.dat$DigitalCaregiverAccount <- as.numeric(as.character(my.dat$DigitalCaregiverAccount))

#Q6.36: I have assisted another adult with digital medical services
my.dat$DigitalAssistMedical = factor(my.dat$DigitalAssistMedical,
                                     levels = c("No",	"Yes"),
                                     labels = c("0",	"1"))
my.dat$DigitalAssistMedical <- as.numeric(as.character(my.dat$DigitalAssistMedical))

#Q6.38: My own separate account(note: the raw values can be retrieved via 'DigitalAssistAccessWays' variable)
my.dat$MedicalLoginInstitution[is.na(my.dat$DigitalAssistAccessWays_institution)] = "2"
my.dat$MedicalLoginInstitution = factor(my.dat$DigitalAssistAccessWays_institution,
                                        levels = c("No","Yes"),
                                        labels = c("0","1"))
my.dat$MedicalLoginInstitution <- as.numeric(as.character(my.dat$MedicalLoginInstitution))

path_to_r_script <- "~/Desktop/R script/my.dat.csv"
write.csv(my.dat, file = path_to_r_script, row.names = FALSE)
