#### Preamble ####
# Purpose: Prepare and clean the CES data downloaded from http://www.ces-eec.ca/
# Author: Amy Chen
# Data: 22 October 2020
# Contact: am.chen@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)

setwd("C:/Users/Amy/Documents/Sta304/Final_project")
# Read in the raw data
raw_ces_data <- read_dta("inputs/2019_Canadian_Election_Study_Online_Survey.dta")
# Add the labels
raw_ces_data <- labelled::to_factor(raw_ces_data)

# Just keep some variables
reduced_ces_data <- 
  raw_ces_data %>% 
  select(vote_intention = cps19_v_likely, 
         cps19_votechoice,
         pes19_votechoice2019,
         age = cps19_age, 
         gender = cps19_gender,
         province = cps19_province,
         education = cps19_education,
         religion = cps19_religion)

#filter out people who don't intend to vote/ not eligible to vote
reduced_ces_data<-
  reduced_ces_data %>% 
  filter(vote_intention != "Unlikely to vote" | !is.na(pes19_votechoice2019)) %>% 
  filter(!is.na(vote_intention) | !is.na(pes19_votechoice2019)) %>% 
  filter(vote_intention != "Certain not to vote" | !is.na(pes19_votechoice2019)) %>% 
  filter(vote_intention != "I am not eligible to vote") %>% 
  filter(pes19_votechoice2019 != "I spoiled my vote" | is.na(pes19_votechoice2019))

#Find vote choice
reduced_ces_data <- 
  reduced_ces_data %>% 
  mutate(pes19_votechoice2019 = 
           ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "Liberal Party", "Liberal Party",
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "Conservative Party", "Conservative Party", 
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "ndp", "NDP", 
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "Green Party", "Green Party", 
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "People's Party", "People's Party", 
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "Another party (please specify)", "Other", 
            ifelse(is.na(pes19_votechoice2019) & cps19_votechoice == "Bloc Qu\xe9b\xe9cois", "Bloc Québécois",
            ifelse(pes19_votechoice2019 == "Liberal Party", "Liberal Party", 
            ifelse(pes19_votechoice2019 == "Conservative Party", "Conservative Party", 
            ifelse(pes19_votechoice2019 == "NDP", "NDP",
            ifelse(pes19_votechoice2019 == "Green Party", "Green Party", 
            ifelse(pes19_votechoice2019 == "People's Party", "People's Party",
            ifelse(pes19_votechoice2019 == "Another party (specify)", "Other", 
            ifelse(pes19_votechoice2019 == "Bloc Québécois", "Bloc Québécois", "Don't know/ Prefer not to answer")))))))))))))))

#filter out people who did not provide a clear vote choice
reduced_ces_data <-
  reduced_ces_data %>% 
  filter(pes19_votechoice2019 != "Don't know/ Prefer not to answer")

#make age groups
reduced_ces_data<-
  reduced_ces_data %>%
  mutate(age_group = 
           ifelse(age %in% 18:24, "18-24", 
                  ifelse(age %in% 25:34, '25-34', 
                         ifelse(age %in% 35:44, '35-44', 
                                ifelse(age %in% 45:54, '45-54', 
                                       ifelse(age %in% 55:64, '55-64', 
                                              ifelse(age %in% 65:74, '65-74', 
                                                     '75+')))))))

# change gender to sex. If it is other change to female.
reduced_ces_data<-
  reduced_ces_data %>%  
  mutate(sex =
           ifelse(gender == "A man", "Male", "Female")) 

# clean up education
reduced_ces_data<-
  reduced_ces_data %>%
  filter(education != "Don't know/ Prefer not to answer") %>% 
  mutate(educ = 
           ifelse(education == "No schooling" | education == "Some elementary school" | education == "Completed elementary school" | education == "Some secondary/ high school", "No certificate, diploma or degree", 
                  ifelse(education == "Completed secondary/ high school" | education == "Some university" | education == "Some technical, community college, CEGEP, College Classique", "HS Diploma or equivalent", 
                         ifelse(education == "Completed technical, community college, CEGEP, College Classique", "Non-university degree",
                                ifelse(education == "Bachelor's degree", "Bachelor's degree", "Degree above Bachelor's")))))
  
# clean up religion
reduced_ces_data <-
  reduced_ces_data %>% 
  mutate(religious = 
           ifelse(religion == "Don't know/ Prefer not to answer" | religion == "None/ Don't have one/ Atheist" | religion == "Agnostic", "No religious affiliation", "Has religious affiliation"))

# clean up province
reduced_ces_data$province <- plyr::revalue(reduced_ces_data$province, c("Northwest Territories" = "Northern Canada"))
reduced_ces_data$province <- plyr::revalue(reduced_ces_data$province, c("Yukon" = "Northern Canada"))
reduced_ces_data$province <- plyr::revalue(reduced_ces_data$province, c("Nunavut" = "Northern Canada"))


# Make vote binary
reduced_ces_data<-
  reduced_ces_data %>%
  mutate(vote_conservative = 
           ifelse(pes19_votechoice2019=="Conservative Party", 1, 0))

reduced_ces_data<-
  reduced_ces_data %>%
  mutate(vote_liberal = 
           ifelse(pes19_votechoice2019=="Liberal Party", 1, 0))

#select the data
ces_data <- 
  reduced_ces_data %>% 
  select(pes19_votechoice2019, vote_conservative, vote_liberal, province, age_group, sex, educ)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(ces_data, "outputs/ces_data.csv")

