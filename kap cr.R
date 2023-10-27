############## KAP for CR SCRIPT #################
############# Author: Prabhjot Juttla

#### Import data set #######
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
install.packages("janitor")
library(janitor)
library(stringr)
library(broom)        
library(lmtest)     
library(parameters)
install.packages("epitools")
library(epitools)
install.packages("ggplot2")
library(ggplot2)
library(readxl)
install.packages("flextable")
install.packages("officer")
library(flextable)
library(officer)
install.packages("countrycode")
library(countrycode)
install.packages("imputeTS")
library(imputeTS)
install.packages("ggpubr")
library(ggpubr)

CR_KAP_FINAL_Responses_ <- read_excel("~/KAP CR/CR KAP FINAL (Responses).xlsx")
View(CR_KAP_FINAL_Responses_)

setwd("C:/Users/Prabhjot/OneDrive/Documents/KAP CR/RCode/KAP CR")

kap_for_covid <- import("CR_KAP_FINAL_Responses_.xlsx")

CR_KAP_FINAL_Responses_ <- as.data.frame(CR_KAP_FINAL_Responses_)

kap_cr <- CR_KAP_FINAL_Responses_

kap_kap <- CR_KAP_FINAL_Responses_


# Rename column names -----------------------------------------------------

kap_cr_clean <- kap_cr %>%
  rename(time = Timestamp,
         consent = `By clicking “I agree” below you are indicating that you are at least 18 years old, have read and understood this consent form and agree to participate in this study`,
    sex = `What is your gender?`,
         age = `How old are you? (indicate completed years, e.g. 54)`,
         cadre = `Please indicate your designation`,
         other_cadre = `If you checked "Other" above, please indicate your designation below.`,
    place_of_work = `Where do you work/study?`,
    years_graduated = `How many years has it been since you graduated (from your undergraduate degree)?`,
    teaching = `Do you partake in any teaching activity at an institution of higher learning?`,
    country_practice = `In which country do you practice (if applicable)? Kindly indicate the name of the country in full.`,
    country_graduate = `In which country did you obtain your undergraduate qualifications (if applicable)? Kindly indicate the name of the country in full.`,
    perceived_knowledge_1 = `How would you rate your knowledge of cardiac rehabilitation?`,
    perceived_knowledge_2 =`How would you rate your knowledge of the multidisciplinary approach to cardiac rehabilitation?`,
    kq_1 = `As of 2022, how many phases have been defined for Cardiac Rehabilitation?`,
    kq_2 = `Cardiac rehabilitation reduces both cardiovascular mortality and episodes of acute hospitalisation.`,
    kq_3 = `Cardiac rehabilitation improves functional capacity and perceived quality of life in patients diagnosed with cardiovascular disease.`,
    kq_4 = `Cardiac rehabilitation is the most clinically and cost-effective therapeutic interventions in cardiovascular disease management`,
    kq_5 = `For which of the following conditions can cardiac rehabilitation be indicated? Tick all that apply.`,
    kq_6 = `For which of the following conditions is cardiac rehabilitation contraindicated? Tick all that apply.`,
    kq_7 = `Members of the multidisciplinary team for cardiac rehabilitation include: ___`,
    kq_8 = `Can a patient who is stable post-Acute Coronary Syndrome be enrolled for Cardiac Rehabilitation?`,
    att_1 = `I consider Cardiac Rehabilitation as an important part of patient management.`,
    att_2 = `I consider Cardiac Rehabilitation to be safe for patients.`,
    att_3 = `I consider myself an important part of the multidisciplinary team for Cardiac Rehabilitation.`,
    att_4 = `Do you think that cardiac rehabilitation programs in Africa would be effective in reducing the burden of Cardiovascular-disease?`,
    att_5 = `Access to a Cardiac Rehabilitation center is an added value in the management of cardiovascular disease in Africa.`,
    att_6 = `Do you think that the outcomes of your patients will be improved if they enrolled in a cardiac rehabilitation program?`,
    att_7 = `If you were diagnosed with a cardiovascular disease for which Cardiac Rehabilitation is indicated, would you participate in a CR program as a patient?`,
    att_q8 = `My patients would be willing to participate in a Cardiac Rehabilitation program.`,
    att_q9_opinion = `Who should initiate Cardiac Rehabilitation? Tick only one.`,
    prac_1 = `When do you refer a patient to begin Cardiac Rehabilitation?`,
    prac_2 = `When I diagnose a patient with cardiovascular disease, I routinely take into consideration the impact the diagnosis has on their mental health.`,
    prac_3 = `My colleagues emphasize pharmacological treatment over lifestyle modifications for the management of cardiovascular disease.`,
    prac_4 = `Is it difficult to refer patients for Cardiac Rehabilitation in the country where you practice?`,
    prac_5 = `In my practice, I emphasize pharmacological treatment over lifestyle modifications for the management of cardiovascular disease.`,
    prac_6 = `How frequently would you send patients to CR if there was a program in place?`,
    prac_7 = `In my experience, the majority of the time, physicians ask patients diagnosed with cardiovascular disease to _______ upon discharge. Choose the answer that is the closest to what you experience.`,
    prac_8 = `The healthcare system of the country in which I practice can support the creation and facilitation of Cardiac Rehabilitation programs.`,
    prac_9 = `The healthcare system of the country in which I obtained my undergraduate qualification can support the creation and facilitation of Cardiac Rehabilitation programs.`,
    barrier_1 = `There exist barriers in referring my patients for cardiac rehabilitation.`,
    barrier_1_explain = `If you checked “Yes” above, what is the most significant barrier you face when referring patients for Cardiac Rehabilitation?`,
    opinion_1_facilitation = `What do you think would facilitate the development of cardiac rehabilitation in your practice?`,
    opinion_2_any = `Kindly share your opinion on any aspect of Cardiac Rehabilitation.`,
    opinion_reduce_burden_kq4 = `Please explain your answer above....27`,
    opinion_reduce_burden_kq5 = `Please explain your answer above....29`,
    opinion_difficult_pq4 = `If you answered "Yes" above, could you please explain why.`)



# DEMOGRAPHICS ------------------------------------------------------------

# SEX ---------------------------------------------------------------------

demo_1 <- kap_cr_clean %>%
  tabyl(sex) %>%
  adorn_pct_formatting()


# AGE ---------------------------------------------------------------------

kap_cr_clean %>%
  tabyl(age) %>%
  adorn_pct_formatting()

mean(kap_cr_clean$age, na.rm = T)
sd(kap_cr_clean$age, na.rm = T)


# Cadre -------------------------------------------------------------------



# Clean cadres  -----------------------------------------------------------

kap_cr_clean$cadre <- gsub("Cardiac surgeon",
                          "Consultant", kap_cr_clean$cadre)

kap_cr_clean$cadre <- gsub("Cardiologist",
                           "Consultant", kap_cr_clean$cadre)



kap_cr_clean$other_cadre <- gsub("Cardiology fellow",
                           "Fellow", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Cardiac sonographer",
                           "Technologist", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Internal medicine",
                                 "Resident (enrolled in a Masters of Medicine or Surgery program)", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Medical student",
                                 "Medical student (currently enrolled in an MBChB program or its equivalent)", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Consultant Physician Internal Medicine",
                           "Consultant", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Consultant Physician Nephrology",
                           "Consultant", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Emergency Physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Family Medicine Physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("family physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Family physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Family Physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Internal medicine physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Internal medicine Physician",
                           "Physician", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Internal medicine resident",
                           "Resident (enrolled in a Masters of Medicine or Surgery program)", kap_cr_clean$other_cadre)

kap_cr_clean$other_cadre <- gsub("Nursing officer",
                           "Nurse", kap_cr_clean$other_cadre)


demo_3 <- kap_cr_clean %>%
  tabyl(cadre)%>%
  adorn_pct_formatting()

demo_4 <- kap_cr_clean %>%
  tabyl(other_cadre)%>%
  adorn_pct_formatting()

#table_output3 <- flextable(demo_3)
#table_output3 <- set_table_properties(table_output3, width = 0.5)
#doc <- read_docx()
#doc <- body_add_flextable(doc, value = table_output3)
#print(doc, target = "demo333.docx")

#table_output4 <- flextable(demo_4)
#table_output4 <- set_table_properties(table_output4, width = 0.5)
#doc <- read_docx()
#doc <- body_add_flextable(doc, value = table_output4)
#print(doc, target = "demo444.docx")


# Facility ----------------------------------------------------------------

demo_5 <- kap_cr_clean %>%
  tabyl(place_of_work)%>%
  adorn_pct_formatting()


# Years since graduated ---------------------------------------------------

demo_6 <- kap_cr_clean %>%
  tabyl(years_graduated)%>%
  adorn_pct_formatting()


# Teaching ----------------------------------------------------------------

demo_7 <- kap_cr_clean %>%
  tabyl(teaching)%>%
  adorn_pct_formatting()


# Country you practise in  ------------------------------------------------

demo_8 <- kap_cr_clean %>%
  tabyl(country_practice)%>%
  adorn_pct_formatting()

####Output should be in the form of continent since the focus is Africa 

kap_cr_clean$country_practice <- as.character(kap_cr_clean$country_practice)

kap_cr_clean$continent <- countrycode(sourcevar = kap_cr_clean[, "country_practice"],
                            origin = "country.name",
                            destination = "continent")



continent_practice <- kap_cr_clean %>%
  tabyl(country_practice)%>%
  adorn_pct_formatting()

# Country you graduated in ------------------------------------------------

demo_9 <- kap_cr_clean %>%
  tabyl(country_graduate)%>%
  adorn_pct_formatting()

kap_cr_clean$continent_grad <- countrycode(sourcevar = kap_cr_clean[, "country_graduate"],
                                      origin = "country.name",
                                      destination = "continent")

continentgrad <- kap_cr_clean %>%
  tabyl(country_practice)%>%
  adorn_pct_formatting()

# KNOWLEDGE ---------------------------------------------------------------


# Perceived knowledge -----------------------------------------------------


# Perceived knowledge Q1-----------------------1 - very poor, 5 = excellent
perceived_1 <- kap_cr_clean %>%
  tabyl(perceived_knowledge_1)%>%
  adorn_pct_formatting()

kap_cr_clean$perceived_knowledge_1 <- gsub("Very poor",
                        "1", kap_cr_clean$perceived_knowledge_1)

kap_cr_clean$perceived_knowledge_1 <- gsub("Poor",
                                           "2", kap_cr_clean$perceived_knowledge_1)

kap_cr_clean$perceived_knowledge_1 <- gsub("Medium",
                                           "3", kap_cr_clean$perceived_knowledge_1)

kap_cr_clean$perceived_knowledge_1 <- gsub("Good",
                                           "4", kap_cr_clean$perceived_knowledge_1)

kap_cr_clean$perceived_knowledge_1 <- gsub("Excellent",
                                           "5", kap_cr_clean$perceived_knowledge_1)

########Proportions 

kap_cr_clean$perceived_knowledge_1 <- as.factor(kap_cr_clean$perceived_knowledge_1)

perceived_knowledge <- kap_cr_clean %>%               
  tabyl(perceived_knowledge_1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# As table in word --------------------------------------------------------


# Perceived knowledge Q2-----------------------1 - very poor, 5 = excellent


kap_cr_clean$perceived_knowledge_2 <- gsub("Very poor",
                                           "1", kap_cr_clean$perceived_knowledge_2)

kap_cr_clean$perceived_knowledge_2 <- gsub("Poor",
                                           "2", kap_cr_clean$perceived_knowledge_2)

kap_cr_clean$perceived_knowledge_2 <- gsub("Medium",
                                           "3", kap_cr_clean$perceived_knowledge_2)

kap_cr_clean$perceived_knowledge_2 <- gsub("Good",
                                           "4", kap_cr_clean$perceived_knowledge_2)

kap_cr_clean$perceived_knowledge_2 <- gsub("Excellent",
                                           "5", kap_cr_clean$perceived_knowledge_2)


########Proportions 
kap_cr_clean$perceived_knowledge_2 <- as.factor(kap_cr_clean$perceived_knowledge_2)



pknow <- kap_cr_clean %>%               
  tabyl(perceived_knowledge_2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

#table_output <- flextable(pknow)
#table_output <- set_table_properties(table_output, width = 0.5)
#doc <- read_docx()
#doc <- body_add_flextable(doc, value = table_output)
#print(doc, target = "perceivedknow2.docx")


# Replace no response with ZERO -------------------------------------------

kap_cr_clean$perceived_knowledge_1 <- as.numeric(kap_cr_clean$perceived_knowledge_1)
kap_cr_clean$perceived_knowledge_2 <- as.numeric(kap_cr_clean$perceived_knowledge_2)


# Total perceived knowledge  ----------------------------------------------

kap_cr_clean$total_perceived <- rowSums(cbind(
                                      kap_cr_clean$perceived_knowledge_1,
                                      kap_cr_clean$perceived_knowledge_2),
                                na.rm = FALSE)

mean(kap_cr_clean$total_perceived, na.rm = T)
sd(kap_cr_clean$total_perceived, na.rm = T)

kap_cr_clean$perceived_percent <- (kap_cr_clean$total_perceived/10)*100

mean(kap_cr_clean$perceived_percent, na.rm = T)


kap_cr_clean$bloom_perceived <- ifelse(test = kap_cr_clean$perceived_percent >= 60,
                               yes = "good",
                               no = "bad")
######New Bloom's Categories
kap_cr_clean$bloom_perceived_new <- ifelse(kap_cr_clean$perceived_percent >= 80, "good",
                                       ifelse(kap_cr_clean$perceived_percent >= 60, "medium", "poor"))



kap_cr_clean %>%               
  tabyl(bloom_perceived) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean %>%               
  tabyl(bloom_perceived_new) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()



# ACTUAL KNOWLEDGE --------------------------------------------------------


# Question 1 = Three = 1 point --------------------------------------------

kap_cr_clean %>%               
  tabyl(kq_1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean$kq_1 <- gsub("Three",
                          "1", kap_cr_clean$kq_1)

kap_cr_clean$kq_1 <- gsub("One",
                          "0", kap_cr_clean$kq_1)

kap_cr_clean$kq_1 <- gsub("Two",
                          "0", kap_cr_clean$kq_1)

kap_cr_clean$kq_1 <- gsub("Four",
                          "0", kap_cr_clean$kq_1)

kap_cr_clean$kq_1 <- gsub("Five",
                          "0", kap_cr_clean$kq_1)

kap_cr_clean$kq_1 <- gsub("I don't know",
                          "0", kap_cr_clean$kq_1)


# Question 2 = TRUE = 1 pnt -----------------------------------------------
kap_cr_clean %>%               
  tabyl(kq_2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean$kq_2 <- gsub("TRUE",
                          "1", kap_cr_clean$kq_2)

kap_cr_clean$kq_2 <- gsub("FALSE",
                          "0", kap_cr_clean$kq_2)

kap_cr_clean$kq_2 <- gsub("I don't know",
                          "0", kap_cr_clean$kq_2)



# Question 3 True--------------------------------------------------------------

kap_cr_clean %>%               
  tabyl(kq_3) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean$kq_3 <- gsub("TRUE",
                          "1", kap_cr_clean$kq_3)

kap_cr_clean$kq_3 <- gsub("FALSE",
                          "0", kap_cr_clean$kq_3)



kap_cr_clean$kq_3 <- gsub("I don't know",
                          "0", kap_cr_clean$kq_3)



# Question 4 = TRUE -------------------------------------------------------

kap_cr_clean %>%               
  tabyl(kq_4) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()




kap_cr_clean$kq_4 <- gsub("TRUE",
                          "1", kap_cr_clean$kq_4)


kap_cr_clean$kq_4 <- gsub("FALSE",
                          "0", kap_cr_clean$kq_4)

kap_cr_clean$kq_4 <- gsub("I don't know",
                          "0", kap_cr_clean$kq_4)


# Question 5  -------------------------------------------------------------
kap_cr_clean %>%               
  tabyl(kq_5) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


kap_cr_clean$kq_5 <- gsub("Acute coronary syndrome",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Coronary revascularisation",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Chronic Heart failure",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Stable angina",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Pre- and post-implantation of cardiac defibrillators and resynchronisation devices",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Post-heart transplantation and ventricular assist devices",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Adult congenital heart disease",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Atrial fibrillation",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Cardiac Transplantation",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Hypertension",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Non-obstructive coronary artery disease",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Peripheral artery disease",
                          "1", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Cerebrovascular accident",
                          "1", kap_cr_clean$kq_5)


kap_cr_clean$kq_5 <- gsub("Rheumatic heart disease",
                          "1", kap_cr_clean$kq_5)


#########\WRONGS
kap_cr_clean$kq_5 <- gsub("Recent thrombophlebitis without pulmonary embolism",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Complex ventricular arrythmias",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Severe pulmonary hypertension",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Intracavitary thrombus",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Prinzmetal angina",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Familial Hypertrophic Cardiomyopathy",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Cardiac amyloidosis",
                          "0", kap_cr_clean$kq_5)


kap_cr_clean$kq_5 <- gsub("Acute decompensated congestive Heart Failure",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Recent thrombophlebitis with pulmonary embolism",
                          "0", kap_cr_clean$kq_5)


kap_cr_clean$kq_5 <- gsub("Severe obstructive myopathies",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Sepsis",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Spontaneous coronary artery dissection",
                          "0", kap_cr_clean$kq_5)

kap_cr_clean$kq_5 <- gsub("Unstable angina",
                          "0", kap_cr_clean$kq_5)


str_count(kap_cr_clean$kq_5, "1")

11 + 11+  6+  8+  7+ 13+  1+  3+  8+  4 +10 + 4 + 7+  4+ 12+  6+  5+ 10+ 8 + 6 + 5 + 4 + 5 + 5+ 6 + 7 + 5 +10 + 5 + 8 + 4 + 5 + 9 +13 + 3 + 6 + 7 + 8 + 3 + 9 + 6 +13+ 13+ 9 + 7 +10 + 7 + 6 +10 + 10 + 12 + 2 + 7 +10 + 5 + 9 + 11 + 6 + 6 + 14 + 13 + 8 + 11 + 11 +  9 + 3 + 4 + 2 + 7 + 9 + 12 + 10 + 7 + 2 + 7 + 10 + 7 + 7 + 11 + 6 + 10 + 5 + 11 + 12 + 4 + 9 + 7 + 5 + 7 + 6 + 8 + 12 + 7 + 6 + 13 + 8
#Total = 730 
#Out of possible 13*104

(730/1352)*100


kap_cr_clean %>%
  tabyl(kq_5)%>%
  adorn_pct_formatting()



# Question 6  -------------------------------------------------------------
kap_cr_clean %>%               
  tabyl(kq_6) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


kap_cr_clean$kq_6 <- gsub("Recent thrombophlebitis without pulmonary embolism",
                          "1", kap_cr_clean$kq_6)


kap_cr_clean$kq_6 <- gsub("Complex ventricular arrythmias",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Severe pulmonary hypertension",
                          "1", kap_cr_clean$kq_6)



kap_cr_clean$kq_6 <- gsub("Intracavitary thrombus",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Prinzmetal angina",
                          "1", kap_cr_clean$kq_6)



kap_cr_clean$kq_6 <- gsub("Familial Hypertrophic Cardiomyopathy",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Cardiac amyloidosis",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Acute decompensated congestive Heart Failure",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Recent thrombophlebitis with pulmonary embolism",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Severe obstructive myopathies",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Sepsis",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Spontaneous coronary artery dissection",
                          "1", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Unstable angina",
                          "1", kap_cr_clean$kq_6)


####\\\\\\
kap_cr_clean$kq_6 <- gsub("Acute coronary syndrome",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Coronary revascularisation",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Chronic Heart failure",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Stable angina",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Pre- and post-implantation of cardiac defibrillators and resynchronisation devices",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Post-heart transplantation and ventricular assist devices",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Adult congenital heart disease",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Atrial fibrillation",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Cardiac Transplantation",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Hypertension",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Non-obstructive coronary artery disease",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Peripheral artery disease",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Cerebrovascular accident",
                          "0", kap_cr_clean$kq_6)

kap_cr_clean$kq_6 <- gsub("Rheumatic heart disease",
                          "0", kap_cr_clean$kq_6)


str_count(kap_cr_clean$kq_6, "1")

1 + 3 + 9 + 5 + 1 + 1 + 2 + 5 + 1 + 3 + 0 + 2 + 2 + 7 + 1 + 3 + 7 + 8 + 3 + 2 + 7 + 5 + 5 + 6 + 8 + 2 + 5 + 5 + 6 + 4 + 1 + 2 + 10 + 2 + 7 + 6 + 4 + 6 + 5 + 3 + 5 + 2 + 7 + 1 + 2 + 12 + 3 + 4 + 5 + 1 + 3 + 2 + 3 + 5 + 8 + 2 + 4 + 2 + 2 + 8 + 5 + 1 + 1 + 7 + 3 + 7 +3 + 7 + 1 + 4 + 2

#Total = 287  
#Out of possible 13*104

(287/1352)*100


# Question 7  -------------------------------------------------------------
kap_cr_clean %>%               
  tabyl(kq_7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean$kq_7 <- gsub("Dietitian",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Exercise specialist",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Occupational therapist",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Pharmacist",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Physician with special interest in prevention and rehabilitation",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Physiotherapist",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Practitioner Psychologist",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Patient",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("1's family",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Speech and language therapists",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Counsellor",
                          "1", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Nurse specialist",
                          "0", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Pathologist",
                          "0", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Laboratory technician",
                          "0", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Research officer",
                          "0", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Health Records Management",
                          "0", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("Behavioral therapists",
                          "0", kap_cr_clean$kq_7)


str_count(kap_cr_clean$kq_7, "1")

9+6+9+9+9+9+5+7+9+9+8+5+9+9+9+8+9+7+9+4+5+3+8+9+5+7+7+9+8+3+9+9+6+6+8+9+9+8+9+9+9+5+6+8+7+1+4+9+9+4+6+9+7+5+8+8+4+8+1+8+9+9+8+3+3+6+7+9+9+8+9+4+9+9+9+9+7+9+8+9+9+9+6+3+9+8+5+9+7+6+9+7+9+9+9

#Total = 697  
#Out of possible 9*104

(697/936)*100

# Question 8 Yes = 1--------------------------------------------------------------
kap_cr_clean %>%               
  tabyl(kq_8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

kap_cr_clean$kq_8 <- gsub("Yes",
                          "1", kap_cr_clean$kq_8)

kap_cr_clean$kq_8 <- gsub("No",
                          "0", kap_cr_clean$kq_8)

kap_cr_clean$kq_8 <- gsub("I don't know",
                          "0", kap_cr_clean$kq_8)



# Separate the numbers in Q5, q6 and q7 ----------------------------------------------------------------

kap_cr_clean$kq_5 <- sapply(strsplit(kap_cr_clean$kq_5,
                                     "[ ,]+"), function(i) sum(as.numeric(i)))

kap_cr_clean$kq_6 <- sapply(strsplit(kap_cr_clean$kq_6,
                                     "[ ,]+"), function(i) sum(as.numeric(i)))

kap_cr_clean$kq_7 <- sapply(strsplit(kap_cr_clean$kq_7,
                                     "[ ,]+"), function(i) sum(as.numeric(i)))



# We need to CAP the max values for q7 coz it is artifically --------
# # # # # #  inflating the knowledge score

kap_cr_clean$kq_7 <- gsub("11",
                          "9", kap_cr_clean$kq_7)

kap_cr_clean$kq_7 <- gsub("10",
                          "9", kap_cr_clean$kq_7)


# Indications  ------------------------------------------------------------

mean(kap_cr_clean$kq_5, na.rm = TRUE)
sd(kap_cr_clean$kq_5, na.rm = TRUE)

mean(kap_cr_clean$kq_6, na.rm = TRUE)
sd(kap_cr_clean$kq_6, na.rm = TRUE)

# Create a total knowledge score column -----------------------------------
kap_cr_clean$kq_1 <- as.numeric(kap_cr_clean$kq_1)
kap_cr_clean$kq_2 <- as.numeric(kap_cr_clean$kq_2)
kap_cr_clean$kq_3 <- as.numeric(kap_cr_clean$kq_3)
kap_cr_clean$kq_4 <- as.numeric(kap_cr_clean$kq_4)
kap_cr_clean$kq_5 <- as.numeric(kap_cr_clean$kq_5)
kap_cr_clean$kq_6 <- as.numeric(kap_cr_clean$kq_6)
kap_cr_clean$kq_7 <- as.numeric(kap_cr_clean$kq_7)
kap_cr_clean$kq_8 <- as.numeric(kap_cr_clean$kq_8)

kap_cr_clean$total_kscore <- rowSums(cbind(kap_cr_clean$kq_1,
                                           kap_cr_clean$kq_2,
                                           kap_cr_clean$kq_3,
                                           kap_cr_clean$kq_4,
                                           kap_cr_clean$kq_5,
                                           kap_cr_clean$kq_6,
                                           kap_cr_clean$kq_7,
                                           kap_cr_clean$kq_8),
                                na.rm = FALSE)



# 40 is the maximum knowledge score ---------------------------------------

kap_cr_clean$percent_knowledge <- ((kap_cr_clean$total_kscore/40)*100)




# Determine mean knowledge score out of 40 possible points  --------------

mean(kap_cr_clean$total_kscore, na.rm = TRUE)
sd(kap_cr_clean$total_kscore, na.rm = TRUE)
summary(kap_cr_clean$total_kscore)

mean(kap_cr_clean$percent_knowledge, na.rm = TRUE)
sd(kap_cr_clean$percent_knowledge, na.rm = TRUE)
summary(kap_cr_clean$percent_knowledge)



# Bloom's cutoff  ---------------------------------------------------------

#kap_cr_clean$bloom_k <- ifelse(test = kap_cr_clean$percent_knowledge >= 60,
#                                 yes = "good",
#                                 no = "bad")

#kap_cr_clean %>%               
#  tabyl(bloom_k) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#####New Bloom's
kap_cr_clean$new_actual_knowledge <- ifelse(kap_cr_clean$percent_knowledge >= 80, "good",
                                           ifelse(kap_cr_clean$percent_knowledge >= 60, "medium", "poor"))



kap_cr_clean %>%               
  tabyl(new_actual_knowledge) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Compare perceived and actual knowledge ----------------------------------

shapiro.test(kap_cr_clean$perceived_percent)
shapiro.test(kap_cr_clean$percent_knowledge)

wilcox.test(kap_cr_clean$perceived_percent,
            kap_cr_clean$percent_knowledge, paired = T)




# ATTITUDE ----------------------------------------------------------------


# Q1, LIKERT SCALE  -------------------------------------------------------

att1 <- kap_cr_clean %>%
  tabyl(att_1) %>%
  adorn_pct_formatting()

table_output <- flextable(att1)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att1.docx")

kap_cr_clean$att_1 <- gsub("Strongly agree",
                            "5", kap_cr_clean$att_1)

kap_cr_clean$att_1 <- gsub("Agree",
                           "4", kap_cr_clean$att_1)


kap_cr_clean$att_1 <- gsub("Neutral",
                           "3", kap_cr_clean$att_1)

kap_cr_clean$att_1 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_1)

kap_cr_clean$att_1 <- gsub("Disagree",
                           "2", kap_cr_clean$att_1)


# Q2  ---------------------------------------------------------------------

att2 <- kap_cr_clean %>%
  tabyl(att_2) %>%
  adorn_pct_formatting()

table_output <- flextable(att2)


table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att2.docx")

kap_cr_clean$att_2 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_2)

kap_cr_clean$att_2 <- gsub("Agree",
                           "4", kap_cr_clean$att_2)

kap_cr_clean$att_2 <- gsub("Neutral",
                           "3", kap_cr_clean$att_2)

kap_cr_clean$att_2 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_2)


# Q3 ----------------------------------------------------------------------

att3 <- kap_cr_clean %>%
  tabyl(att_3) %>%
  adorn_pct_formatting()

table_output <- flextable(att3)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att3.docx")

kap_cr_clean$att_3 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_3)

kap_cr_clean$att_3 <- gsub("Agree",
                           "4", kap_cr_clean$att_3)

kap_cr_clean$att_3 <- gsub("Neutral",
                           "3", kap_cr_clean$att_3)

kap_cr_clean$att_3 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_3)

kap_cr_clean$att_3 <- gsub("Disagree",
                           "2", kap_cr_clean$att_3)


# Question 4 --------------------------------------------------------------

att4 <- kap_cr_clean %>%
  tabyl(att_4) %>%
  adorn_pct_formatting()


table_output <- flextable(att4)
table_output <- set_table_properties(table_output, width = 0.5)

doc <- read_docx()

doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att4.docx")

kap_cr_clean$att_4 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_4)

kap_cr_clean$att_4 <- gsub("Agree",
                           "4", kap_cr_clean$att_4)

kap_cr_clean$att_4 <- gsub("Neutral",
                           "3", kap_cr_clean$att_4)

kap_cr_clean$att_4 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_4)

kap_cr_clean$att_4 <- gsub("Disagree",
                           "2", kap_cr_clean$att_4)



# q5 ----------------------------------------------------------------------

att5 <- kap_cr_clean %>%
  tabyl(att_5) %>%
  adorn_pct_formatting()

table_output <- flextable(att5)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att5.docx")

kap_cr_clean$att_5 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_5)

kap_cr_clean$att_5 <- gsub("Agree",
                           "4", kap_cr_clean$att_5)

kap_cr_clean$att_5 <- gsub("Neutral",
                           "3", kap_cr_clean$att_5)

kap_cr_clean$att_5 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_5)

kap_cr_clean$att_5 <- gsub("Disagree",
                           "2", kap_cr_clean$att_5)



# Q6 ----------------------------------------------------------------------

att6 <- kap_cr_clean %>%
  tabyl(att_6) %>%
  adorn_pct_formatting()

table_output <- flextable(att6)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att6.docx")

kap_cr_clean$att_6 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_6)

kap_cr_clean$att_6 <- gsub("Agree",
                           "4", kap_cr_clean$att_6)

kap_cr_clean$att_6 <- gsub("Neutral",
                           "3", kap_cr_clean$att_6)

kap_cr_clean$att_6 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_6)

kap_cr_clean$att_6 <- gsub("Disagree",
                           "2", kap_cr_clean$att_6)


# Q7 ----------------------------------------------------------------------

att7 <- kap_cr_clean %>%
  tabyl(att_7) %>%
  adorn_pct_formatting()

table_output <- flextable(att7)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att7.docx")

kap_cr_clean$att_7 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_7)

kap_cr_clean$att_7 <- gsub("Agree",
                           "4", kap_cr_clean$att_7)

kap_cr_clean$att_7 <- gsub("Neutral",
                           "3", kap_cr_clean$att_7)

kap_cr_clean$att_7 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_7)

kap_cr_clean$att_7 <- gsub("Disagree",
                           "2", kap_cr_clean$att_7)


# Question 8  -------------------------------------------------------------

att8 <- kap_cr_clean %>%
  tabyl(att_q8) %>%
  adorn_pct_formatting()

table_output <- flextable(att8)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att8.docx")

kap_cr_clean$att_q8 <- gsub("Strongly agree",
                           "5", kap_cr_clean$att_q8)

kap_cr_clean$att_q8 <- gsub("Agree",
                           "4", kap_cr_clean$att_q8)

kap_cr_clean$att_q8 <- gsub("Neutral",
                           "3", kap_cr_clean$att_q8)

kap_cr_clean$att_q8 <- gsub("Strongly disagree",
                           "1", kap_cr_clean$att_q8)

kap_cr_clean$att_q8 <- gsub("Disagree",
                           "2", kap_cr_clean$att_q8)


# Question 9  opinion-------------------------------------------------------------

att9 <- kap_cr_clean %>%
  tabyl(att_q9_opinion) %>%
  adorn_pct_formatting()

table_output <- flextable(att9)
table_output <- set_table_properties(table_output, width = 0.5)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table_output)
print(doc, target = "att9.docx")


# Attitude out of 40 possible score ---------------------------------------
kap_cr_clean$att_1 <- as.numeric(kap_cr_clean$att_1)
kap_cr_clean$att_2 <- as.numeric(kap_cr_clean$att_2)
kap_cr_clean$att_3 <- as.numeric(kap_cr_clean$att_3)
kap_cr_clean$att_4 <- as.numeric(kap_cr_clean$att_4)

kap_cr_clean$att_5 <- as.numeric(kap_cr_clean$att_5)
kap_cr_clean$att_6 <- as.numeric(kap_cr_clean$att_6)

kap_cr_clean$att_7 <- as.numeric(kap_cr_clean$att_7)
kap_cr_clean$att_q8 <- as.numeric(kap_cr_clean$att_q8)

kap_cr_clean$total_attscore <- rowSums(cbind(kap_cr_clean$att_1,
                                           kap_cr_clean$att_2,
                                           kap_cr_clean$att_3,
                                           kap_cr_clean$att_4,
                                           kap_cr_clean$att_5,
                                           kap_cr_clean$att_6,
                                           kap_cr_clean$att_7,
                                           kap_cr_clean$att_q8),
                                     na.rm = FALSE)


# 40 is the maximum attitude score ---------------------------------------

kap_cr_clean$percent_att <- ((kap_cr_clean$total_attscore/40)*100)


# Determine mean knowledge score out of 40 possible points  --------------

mean(kap_cr_clean$total_attscore, na.rm = TRUE)
sd(kap_cr_clean$total_attscore, na.rm = TRUE)
summary(kap_cr_clean$total_attscore)


# Bloom's cutoff  ---------------------------------------------------------

#kap_cr_clean$bloom_att <- ifelse(test = kap_cr_clean$percent_att >= 60,
#                               yes = "good",
#                               no = "bad")

#kap_cr_clean %>%               
#  tabyl(bloom_att) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()


######New Bloom's  cutoff

kap_cr_clean$new_bloom_att <- ifelse(kap_cr_clean$percent_att >= 80, "good",
                                            ifelse(kap_cr_clean$percent_att >= 60, "medium", "poor"))



kap_cr_clean %>%               
  tabyl(new_bloom_att) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# PRACTICE QUESTIONS ------------------------------------------------------

# Practice Q1 -------------------------------------------------------------
kap_cr_clean$prac_1[kap_cr_clean$prac_1 == "Not applicable (not in a referral role)"] <- NA

kap_cr_clean %>%
  tabyl(prac_1) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_1 <- gsub("Starting in the hospital settings",
                           "1", kap_cr_clean$prac_1)

kap_cr_clean$prac_1 <- gsub("4 weeks or more after the discharge",
                            "0", kap_cr_clean$prac_1)

kap_cr_clean$prac_1 <- gsub("Directly after the hospital discharge",
                            "1", kap_cr_clean$prac_1)

kap_cr_clean$prac_1 <- gsub("I do not refer my patients for Cardiac Rehabilitation",
                            "0", kap_cr_clean$prac_1)



# Practice 2 --------------------------------------------------------------

kap_cr_clean %>%
  tabyl(prac_2) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_2 <- gsub("Strongly agree",
                            "5", kap_cr_clean$prac_2)

kap_cr_clean$prac_2 <- gsub("Agree",
                            "4", kap_cr_clean$prac_2)

kap_cr_clean$prac_2 <- gsub("Neutral",
                            "3", kap_cr_clean$prac_2)

kap_cr_clean$prac_2 <- gsub("Strongly disagree",
                            "1", kap_cr_clean$prac_2)

kap_cr_clean$prac_2 <- gsub("Disagree",
                            "2", kap_cr_clean$prac_2)



# Q3 ----------------------------------------------------------------------

kap_cr_clean$prac_3[kap_cr_clean$prac_3 == "Not applicable"] <- NA

kap_cr_clean %>%
  tabyl(prac_3) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_3 <- gsub("FALSE",
                            "1", kap_cr_clean$prac_3)

kap_cr_clean$prac_3 <- gsub("TRUE",
                            "0", kap_cr_clean$prac_3)


# Question 4 opinion ------------------------------------------------------

kap_cr_clean$prac_4[kap_cr_clean$prac_4 == "Not applicable (not in a referral role)"] <- NA

kap_cr_clean %>%
  tabyl(prac_4) %>%
  adorn_pct_formatting()



# Question 5 --------------------------------------------------------------

kap_cr_clean$prac_5[kap_cr_clean$prac_5 == "Not applicable"] <- NA

kap_cr_clean %>%
  tabyl(prac_5) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_5 <- gsub("FALSE",
                            "1", kap_cr_clean$prac_5)

kap_cr_clean$prac_5 <- gsub("TRUE",
                            "0", kap_cr_clean$prac_5)




# Question 6  -------------------------------------------------------------

kap_cr_clean %>%
  tabyl(prac_6) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_6 <- gsub("3 to 5 times per week",
                            "1", kap_cr_clean$prac_6)

kap_cr_clean$prac_6 <- gsub("Other",
                            "1", kap_cr_clean$prac_6)

kap_cr_clean$prac_6 <- gsub("1 to 2 times per month",
                            "0", kap_cr_clean$prac_6)

kap_cr_clean$prac_6 <- gsub("3 to 5 times a month",
                            "0", kap_cr_clean$prac_6)

kap_cr_clean$prac_6 <- gsub("Never",
                            "0", kap_cr_clean$prac_6)

kap_cr_clean$prac_6 <- gsub("Once every 6 months",
                            "0", kap_cr_clean$prac_6)



# Question 7 --------------------------------------------------------------

kap_cr_clean %>%
  tabyl(prac_7) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_7 <- gsub("start a rehab program",
                            "1", kap_cr_clean$prac_7)

kap_cr_clean$prac_7 <- gsub("avoid straining themselves",
                            "0", kap_cr_clean$prac_7)

kap_cr_clean$prac_7 <- gsub("do nothing and just rest",
                            "0", kap_cr_clean$prac_7)

kap_cr_clean$prac_7 <- gsub("exercise a bit",
                            "0", kap_cr_clean$prac_7)

kap_cr_clean$prac_7 <- gsub("exercise as tolerated",
                            "0", kap_cr_clean$prac_7)

kap_cr_clean$prac_7 <- gsub("go to fitness club",
                            "0", kap_cr_clean$prac_7)


# Total Practice points ---------------------------------------------------

kap_cr_clean$prac_1 <- as.numeric(kap_cr_clean$prac_1)
kap_cr_clean$prac_2 <- as.numeric(kap_cr_clean$prac_2)
kap_cr_clean$prac_3 <- as.numeric(kap_cr_clean$prac_3)
kap_cr_clean$prac_5 <- as.numeric(kap_cr_clean$prac_5)
kap_cr_clean$prac_6 <- as.numeric(kap_cr_clean$prac_6)
kap_cr_clean$prac_7 <- as.numeric(kap_cr_clean$prac_7)


kap_cr_clean$total_prac_score <- rowSums(cbind(kap_cr_clean$prac_1,
                                             kap_cr_clean$prac_2,
                                             kap_cr_clean$prac_3,
                                             kap_cr_clean$prac_5,
                                             kap_cr_clean$prac_6,
                                             kap_cr_clean$prac_7),
                                       na.rm = FALSE)


# 10 is the maximum practice score ---------------------------------------

kap_cr_clean$percent_prac <- ((kap_cr_clean$total_prac_score/10)*100)


# Determine mean knowledge score out of 40 possible points  --------------

mean(kap_cr_clean$total_prac_score, na.rm = TRUE)
sd(kap_cr_clean$total_prac_score, na.rm = TRUE)


mean(kap_cr_clean$percent_prac, na.rm = TRUE)

# Bloom's cutoff  ---------------------------------------------------------

#kap_cr_clean$bloom_PRAC <- ifelse(test = kap_cr_clean$percent_prac >= 60,
#                                 yes = "good",
#                                 no = "bad")

#kap_cr_clean %>%               
#  tabyl(bloom_PRAC) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()


kap_cr_clean$new_bloom_prac <- ifelse(kap_cr_clean$percent_prac >= 80, "good",
                                     ifelse(kap_cr_clean$percent_prac >= 60, "medium", "poor"))



kap_cr_clean %>%               
  tabyl(new_bloom_prac) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Opinion on practice q8 and q9--------------------------------------------------

kap_cr_clean$prac_8[kap_cr_clean$prac_8 == "Not applicable (not in a referral role)"] <- NA

kap_cr_clean %>%
  tabyl(prac_8) %>%
  adorn_pct_formatting()

kap_cr_clean$prac_9[kap_cr_clean$prac_9 == "I have not graduated"] <- NA

kap_cr_clean %>%
  tabyl(prac_9) %>%
  adorn_pct_formatting()



# Barriers to CR ----------------------------------------------------------
kap_cr_clean$barrier_1[kap_cr_clean$barrier_1 == "Not applicable (I do not refer patients)"] <- NA

kap_cr_clean %>%
  tabyl(barrier_1) %>%
  adorn_pct_formatting()

kap_cr_clean %>%
  tabyl(barrier_1_explain) %>%
  adorn_pct_formatting()


# Separate the barriers ---------------------------------------------------

barrier_analysis <- kap_cr_clean

unlist(strsplit(barrier_analysis$barrier_1_explain,","))


str1 = barrier_analysis$barrier_1_explain
ch1 = "Lack of specialists"
ch2 = "Lack of knowledge"
ch3 = " Lack of motivation"
ch4 = " Cost of care"
ch5 = " Localization of the centre"
ch6 = " All of the above"
ch7 = "None of the above"
ch8 = "Not applicable"

str_count(str1, ch1)
###
1+1+1+ 1 + 1+ 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
(38/160)*100

str_count(str1, ch2)
###
3 + 14+ 15 + 6

str_count(str1, ch3)
###
4+10+3

str_count(str1, ch4)
###
14+11+5

str_count(str1, ch5)
###
1+9+9+5

str_count(str1, ch6)
###
3+4+1

str_count(str1, ch7)
### 1

str_count(str1, ch8)
####
3+1


38+38+17+30+24+8+5

(17/160)*100
(30/160)*100
(24/160)*100
(8/160)*100
(1/160)*100
(4/160)*100

kap_cr_clean %>%
  tabyl(opinion_2_any) %>%
  adorn_pct_formatting()



# To word -----------------------------------------------------------------

table_output <- flextable(demo_9)

# Optionally, you can apply some formatting to the table
table_output <- set_table_properties(table_output, width = 0.5)

# Create a Word document
doc <- read_docx()

# Add the flextable to the Word document
doc <- body_add_flextable(doc, value = table_output)

# Save the Word document
print(doc, target = "output_table7.docx")


# GRAPHS  -----------------------------------------------------------------
#####Column graphs
library(tidyverse)

perceiveddd <- data.frame(Percentage = c(62.8, 56.9),
                         Responses = c("Perceived", "Actual"))
  
knowvs <- ggplot(perceiveddd, 
                    aes(x = Responses, y = Percentage, 
                        fill = Responses)) +
  geom_bar(width = .3, stat = "Identity", size = 0.1) +
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .3, position = position_dodge(.1),
           color = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 100)) +
  labs(
    x = " ",
    y = "Percentage",
    title = "Perceived Vs. Actual CR Knowledge") +
  theme(plot.title = element_text(size = 30, face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 15)),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 25, color = "black",
                                  face = "bold"),
        axis.text = element_text(size = 25, color = "black"),
        axis.text.x = element_text(margin = margin(t = 18)),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(margin = margin(r = 22)),
        axis.ticks.x = element_blank(),
        legend.position = "none") + scale_x_discrete(limits = c('Perceived', "Actual"))

significant <- c(1, 0)

newknow <- knowvs + geom_text(aes(label = ifelse(significant == 1, "*", "")),
                   position = position_dodge(width = 0.6), vjust = -1, size = 15) +
  geom_errorbar(aes(ymin = ifelse(significant == 1, 0, NA),
                  ymax = ifelse(significant == 1, -10, NA)),
              position = position_dodge(width = 0.3), width = 0.1)

knowvs

ggsave(knowvs, filename = "perceived.png",
       width=20, height=12, dpi = 300)


# Discharge instructions --------------------------------------------------


discharggeee <- data.frame(Percentage = c(60.9, 13, 12, 8.7, 4.3, 1.1),
                           Responses = c("Exercise as tolerated",
                                         "Exercise a bit", 
                                         "Start a Rehab Program",
                                         "Do Nothing & Rest",
                                         "Avoid Straining",
                                         "Fitness Club"))


dx <- ggplot(discharggeee, 
                 aes(x = Responses, y = Percentage, 
                     fill = Responses)) +
  geom_bar(width = .3, stat = "Identity", size = 0.1) +
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .3, position = position_dodge(.1),
           color = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 100)) +
  labs(
    x = " ",
    y = "Percentage",
    title = "Most Common Instructions Upon Discharge") +
  theme(plot.title = element_text(size = 30, face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 15)),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 22, color = "black",
                                  face = "bold"),
        axis.text = element_text(size = 17, color = "black"),
        axis.text.x = element_text(margin = margin(t = 18)),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(margin = margin(r = 22)),
        axis.ticks.x = element_blank(),
        legend.position = "none") + scale_x_discrete(limits = c("Exercise as tolerated",
                                                                "Exercise a bit", 
                                                                "Start a Rehab Program",
                                                                "Do Nothing & Rest",
                                                                "Avoid Straining",
                                                                "Fitness Club"))

dx

ggsave(dx, filename = "dx.png",
       width=20, height=12, dpi = 300)


# Instructions  -----------------------------------------------------------
recommddd <- data.frame(Percentage = c(30.8, 27.5, 26.4, 6.6, 2.2),
                             Responses = c("3-5 times a month",
                                           "1-2 times per month",
                                           "3-5 times per week",
                                           "Never",
                                           "Once every 6 months"))
wrongpraccc <- ggplot(recommddd, 
             aes(x = Responses, y = Percentage, 
                 fill = Responses)) +
  geom_bar(width = .3, stat = "Identity", size = 0.1) +
  scale_fill_brewer() +
  theme_classic() +
  geom_col(width = .3, position = position_dodge(.1),
           color = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 100)) +
  labs(
    x = " ",
    y = "Percentage",
    title = "How often would you send a patient for CR?") +
  theme(plot.title = element_text(size = 30, face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 15)),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 25, color = "black",
                                  face = "bold"),
        axis.text = element_text(size = 22, color = "black"),
        axis.text.x = element_text(margin = margin(t = 18)),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(margin = margin(r = 22)),
        axis.ticks.x = element_blank(),
        legend.position = "none") + scale_x_discrete(limits = c("3-5 times a month",
                                                                "1-2 times per month",
                                                                "3-5 times per week",
                                                                "Never",
                                                                "Once every 6 months"))

wrongpraccc

ggsave(wrongpraccc, filename = "pracccc.png",
       width=20, height=12, dpi = 300)


# All graphs  -------------------------------------------------------------

responses <- ggarrange(newknow,
                       pie_chart,
                      wrongpraccc,
                      pie_chart_att,
                      dx,
                      pie_chart_prac,
                    labels = c("A", "D", "B", "E", "C",   "F"),
                       font.label = list(size = 35, color = "black", face = "bold"),
                       ncol = 2, nrow = 3)

ggsave(responses, filename = "composite_full.png",
       width = 25, height = 20, dpi = 300)


# Can support -------------------------------------------------------------
Graduate <- data.frame(Percentage = c(85.4, 14.6),
                          Responses = c("Yes", "No"))

Practice <- data.frame(Percentage = c(77.2, 22.8),
                       Responses = c("Yes", "No"))

p = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = Percentage, fill = Graduate), stat = "identity", color = "white") + 
  coord_polar("y", start = 0)






# KAP Pie chart -----------------------------------------------------------

install.packages("plotly")
library(plotly)


# Data
# Data
# Load required libraries
library(ggplot2)

# Data
labels <- c("Good Knowledge", "Medium Knowledge", "Poor Knowledge")
percentages <- c(4.0, 45.3, 50.7) # Percentages
counts <- c(3, 34, 38) # Corresponding counts

# Create a data frame
data <- data.frame(labels, percentages, counts)

# Create a pie chart using ggplot
pie_chart <- ggplot(data, aes(x = "", y = percentages, fill = labels)) +
  scale_fill_brewer() +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Knowledge", fill = "Bloom's cutoff") +
  geom_text(aes(label = paste0(percentages, "% (n = ", counts, ")")), position = position_stack(vjust = 0.5)) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    panel.background = element_rect(fill = "white")
  )

# Save the ggplot object using ggsave
ggsave("pie_chart_know_final.png", plot = pie_chart, width = 6, height = 6, dpi = 300)


##########Attitude

# Data
labels_att <- c("Good Attitude", "Medium Attitude", "Poor Attitude")
percentages_att <- c(87.0, 13.0, 0.0) # Percentages
counts_att <- c(80, 20, 0) # Corresponding counts

# Create a data frame
data_att <- data.frame(labels_att, percentages_att, counts_att)

# Create a pie chart using ggplot2
# Load required libraries
library(ggplot2)

# Data
labels_att <- c("Good Attitude", "Medium Attitude", "Poor Attitude")
percentages_att <- c(87.0, 13.0, 0.0) # Percentages
counts_att <- c(80, 20, 0) # Corresponding counts

# Create a data frame
data_att <- data.frame(labels_att, percentages_att, counts_att)

# Create a pie chart using ggplot2
pie_chart_att <- ggplot(data_att, aes(x = "", y = percentages_att, fill = labels_att)) +
  scale_fill_brewer() +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Attitude", fill = "Bloom's cutoff") +
  geom_text(aes(label = paste0(percentages_att, "% (n = ", counts_att, ")")), position = position_stack(vjust = 0.5)) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    panel.background = element_rect(fill = "white"))

pie_chart_att


# Save the ggplot object using ggsave
ggsave("pie_chart_att_final.png", plot = pie_chart_att, width = 6, height = 6, dpi = 300)


##########Practice

# Data
labels_prac <- c("Good Practice", "Medium Practice", "Poor Practice")
percentages_prac <- c(29.7, 57.8, 12.5) # Percentages
counts_prac <- c(19, 37, 8) # Corresponding counts


# Create a data frame
data_prac <- data.frame(labels_prac, percentages_prac, counts_prac)

# Create a pie chart using ggplot2
pie_chart_prac <- ggplot(data_prac, aes(x = "", y = percentages_prac, fill = labels_prac)) +
  scale_fill_brewer() +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Practice", fill = "Bloom's cutoff") +
  geom_text(aes(label = paste0(percentages, "% (n = ", counts, ")")), position = position_stack(vjust = 0.5)) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    panel.background = element_rect(fill = "white"))

pie_chart_prac

# Save the ggplot object using ggsave
ggsave("pie_chart_att_final.png", plot = pie_chart_prac, width = 6, height = 6, dpi = 300)


###### Composite KAP 
kap <- ggarrange(
                       pie_chart,
                       
                       pie_chart_att,
                       
                       pie_chart_prac,
                       labels = c("A", "B", "C"),
                       font.label = list(size = 35, color = "black", face = "bold"),
                       ncol = 3, nrow = 1)

ggsave(kap, filename = "composite_full.png",
       width = 25, height = , dpi = 300)

