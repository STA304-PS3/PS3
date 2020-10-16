#### Preamble ####
# Purpose: Now that the Data set has been cleaned and labelled by using the code provided by Alexander 
# and Caetano (Alexander Caetano 2020). The next step is to pull the information important to our analysis
# of Canadian women's decision making on the number of children they wish to have. This will be done by
# creating a subset of the total population that we neeed (women who filled out all the variables of interest)
# and by selecting and transforming all of the data in to more useful variables.
# Authors: Ben Draskovic
# Date: 16 October 2020
# License: MIT

# Pre-reqs: You need to have run gss_cleaning-1.R from the associated git repository. 

#### Workspace Set Up ####
library(tidyverse)
library(janitor)


#### Pull Data, select Important Variables, and Create Subset ####

gss_used <- read.csv(file = "Outputs/gss.csv")


gss_used <- gss_used %>%  #select all important variables 
  rename(religion_importance = regilion_importance) %>% 
  select(
    caseid,
    sex,
    total_children,
    age,
    age_at_first_birth,
    religion_importance,
    income_family,
    occupation,
    education,
    education,
    partner_sex,
    region,
    pop_center,
  ) 

gss_prepped <- gss_used %>% 
  subset(sex == "Female") %>%   #removes all male respondants a total of 9399
  filter(!is.na(total_children), #removes all respondants with nas in critical variables a total of 287
         !is.na(age), 
         !is.na(religion_importance), 
         !is.na(income_family), 
         !is.na(education),
         !is.na(partner_sex),
         !is.na(region),
         !is.na(pop_center)
         )

#### Adjust Variables to be more Usable ####

#Create Decade of first birth, and group both types of ages 
gss_prepped <- gss_prepped %>% 
  mutate(decade_first_birth = 2017-age+age_at_first_birth ) %>% 
  mutate(decade_first_birth = (as.character(cut(decade_first_birth, 
                                breaks = c(seq(1969, 2019, by = 10), Inf),
                                labels = seq(1960, 2010, by = 10), right = FALSE
          ))), 
          age_group = (as.character(cut(age, 
                                breaks = c(seq(15, 80, by = 5), Inf),
                                labels = seq(15, 80, by = 5), right = FALSE
          ))),
         age_at_first_birth_group = (as.character(cut(age_at_first_birth, 
                                breaks = c(seq(15, 80, by = 5), Inf),
                                labels = seq(15, 80, by = 5), right = FALSE
         ))),)

#Trying to group the education levels 
#gss_prepped$education <- gsub("	Bachelor's degree (e.g. B.A., B.Sc., LL.B.)", "BA/College", gss_prepped$education)


write_csv(gss, "Outputs/gss-prepared-for-analysis.csv")

