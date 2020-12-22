####Preamble####
# Purpose: Prepare and clean the Census of Canada, 2016 data downloaded from the CHASS website
# Author: Amy Chen
# Data: 22 October 2020
# Contact: am.chen@mail.utoronto.ca
# License: MIT

#### Workspace set-up ####
library(janitor)
library(haven)
library(tidyverse)
setwd("C:/Users/Amy/Documents/Sta304/Final_project")
raw_census_data <- read_csv("inputs/census.csv")

#Filter out people who are too young to vote
census_data <-
  raw_census_data %>% 
  filter(agegrp != "0 to 4 years") %>% 
  filter(agegrp != "5 to 6 years") %>% 
  filter(agegrp != "7 to 9 years") %>% 
  filter(agegrp != "10 to 11 years") %>% 
  filter(agegrp != "12 to 14 years") %>% 
  filter(agegrp != "15 to 17 years") %>% 
  filter(agegrp != "Not available")

#Filter for people who are non-immigrants (citizens by birth) and immigrants (immigrants/permanent residents)
census_data <- 
  census_data %>% 
  filter(immstat != "Non-permanent residents") %>% 
  filter(immstat != "Not available")

#make age groups
census_data<-
  census_data %>%
  mutate(age_group = 
           ifelse(agegrp == "18 to 19 years" | agegrp== "20 to 24 years", "18-24", 
                  ifelse(agegrp == "25 to 29 years" | agegrp == "30 to 34 years", '25-34', 
                         ifelse(agegrp == "35 to 39 years" | agegrp == "40 to 44 years", '35-44', 
                                ifelse(agegrp == "45 to 49 years" | agegrp == "50 to 54 years", '45-54', 
                                       ifelse(agegrp == "55 to 59 years" | agegrp == "60 to 64 years", '55-64', 
                                              ifelse(agegrp == "65 to 69 years " | agegrp == "70 to 74 years", '65-74', 
                                                     '75+')))))))

#make education groups
census_data<-
  census_data %>%
  filter(hdgree != "Not available") %>% 
  mutate(educ = 
           ifelse(hdgree == "No certificate, diploma or degree", "No certificate, diploma or degree", 
                  ifelse(hdgree == "Secondary (high) school diploma or equivalency certificate", "HS Diploma or equivalent", 
                                ifelse(hdgree == "Certificate of Apprenticeship or Certificate of Qualification"| hdgree == "Program of 1 to 2 years (College, CEGEP and other non-university certificates or diplomas)"
                                       | hdgree == "Trades certificate or diploma other than Certificate of Apprenticeship or Certificate of Qualification"
                                       | hdgree == "Program of more than 2 years (College, CEGEP and other non-university certificates or diplomas)"
                                       | hdgree == "Program of 3 months to less than 1 year (College, CEGEP and other non-university certificates or diplomas)"
                                       | hdgree == "University certificate or diploma below bachelor level", "Non-university degree",
                                       ifelse(hdgree == "Bachelor's degree", "Bachelor's degree", "Degree above Bachelor's")))))

# rename
census_data <- 
  census_data %>% 
  select(age_group, 
         sex, 
         educ,
         province = pr)

#splitting the cells
census_cells <- 
  census_data %>%
  count(age_group, sex, educ, province) %>%
  group_by(age_group, sex, educ, province) 


write_csv(census_cells, "outputs/census_data.csv")
