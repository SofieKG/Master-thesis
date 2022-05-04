# Transforming dataset 
rm(list = ls()) # removing whatever is in environment.

# Installing all necessary packages:
install.packages("multiwayvcov", "sandwhich", "plm", "stargazer", 
                 "dplyr", "base", "car", "cluster",
                 "estimatr", "ggplot2", "lmtest", "readxl",
                 "tidyverse", "moments")

library(multiwayvcov, sandwhich, plm, stargazer, 
        dplyr, base, car, cluster,
        estimatr, ggplot2, lmtest, readxl,
        tidyverse, moments) 

# Loading dataset
igdata <- read_excel("~/OneDrive - Universitetet i Oslo/Masteroppgåve/Writing/Chapter 5/R/Merged_excel_R.xlsx")
view(igdata)

# Decide I need to transform the wide variables to long ones:

# Transforming idealposition to long
ideal_data <- igdata %>% 
  select(groupname, category, category_numeric, fte, origin_country, brussels_office, 
         starts_with("ideal")) %>% 
  pivot_longer(starts_with("ideal"), values_to = "ideal_value",
               names_to = "issue_name") %>% 
  mutate(issue = str_remove(issue_name, "ideal_")) %>% 
  select(-issue_name)


# Transforming coalition to long
coal_data <- igdata %>% 
  select(groupname, category, category_numeric, fte, origin_country, brussels_office, 
         starts_with("coal")) %>% 
  pivot_longer(starts_with("coal"), values_to = "coal_value",
               names_to = "issue_name") %>% 
  mutate(issue = str_remove(issue_name, "coal_")) %>% 
  select(-issue_name)


# Transforming salience to long
sal_data <- igdata %>% 
  select(groupname, category, category_numeric, fte, origin_country, brussels_office, 
         starts_with("salience")) %>%
  pivot_longer(starts_with("salience"), values_to = "sal_value",
               names_to = "issue_name") %>% 
  mutate(issue = str_remove(issue_name, "salience_")) %>% 
  select(-issue_name)


# Transforming SQ to long
SQ_data <- igdata %>% 
  select(groupname, category, category_numeric, fte, origin_country, brussels_office, 
         starts_with("SQ")) %>% 
  pivot_longer(starts_with("SQ"), values_to = "SQ_value",
               names_to = "issue_name") %>% 
  mutate(issue = str_remove(issue_name, "SQ_")) %>% 
  select(-issue_name)


# Transforming FO to long
FO_data <- igdata %>% 
  select(groupname, category, category_numeric, fte, origin_country, brussels_office, 
         starts_with("FO")) %>% 
  pivot_longer(starts_with("FO"), values_to = "FO_value",
               names_to = "issue_name") %>% 
  mutate(issue = str_remove(issue_name, "FO_")) %>% 
  select(-issue_name)


# Combining the new datasets:

alldata <- full_join(ideal_data, coal_data, by = c("groupname", "issue", 
                                                   "category", "category_numeric", 
                                                   "fte", "origin_country", "brussels_office"))

alldata2 <- full_join(FO_data, SQ_data, by = c("groupname", "issue", 
                                                "category", "category_numeric", 
                                                "fte", "origin_country", "brussels_office"))

alldata3 <- full_join(alldata, alldata2, by = c("groupname", "issue", 
                                               "category", "category_numeric", 
                                               "fte", "origin_country", "brussels_office"))

alldata_final <- full_join(alldata3, sal_data, by = c("groupname", "issue", 
                                             "category", "category_numeric", 
                                             "fte", "origin_country", "brussels_office"))

### Dependent variable of success ###

# Create new variable for success in the dataset based on the mathematic formula for success:
alldata_final <- alldata_final %>%
  mutate(success = abs(abs(alldata_final$ideal_value - alldata_final$SQ_value) - 
                         abs(alldata_final$ideal_value - alldata_final$FO_value)))

# Running a test for a sinlge row just to make sure everything works: 
abs(alldata_final[6, "ideal_value"] - alldata_final[6, "SQ_value"]) -
  (abs(alldata_final[6, "ideal_value"] - alldata_final[6, "FO_value"]))


# Creating new dataset where all NA's are removed, only keeping those rows with a value on ideal_value:
sdata <- alldata_final %>% 
  filter(is.na(ideal_value) == FALSE)

save(sdata, file = "sdata_final.RData") #saving the dataset so I can work with this from now on.

rm(list=ls()) # removing everything in environment.

load("~/sdata_final.RData") # loading data to environment.

# Transforming the origin_country variable to one with only three categories
# This is in order to test whether being from an "old" MS is an advantage when lobbying the EC.

sdata <- sdata %>%
  mutate(origin_country_new = case_when(origin_country == "Belgium" ~ 0, # 0 = pre-2004 EU members
                                        origin_country == "Spain" ~ 0,
                                        origin_country == "France" ~ 0,
                                        origin_country == "Sweden" ~ 0,
                                        origin_country == "Italy" ~ 0,
                                        origin_country == "Germany" ~ 0,
                                        origin_country == "Denmark" ~ 0,
                                        origin_country == "Finland" ~ 0,
                                        origin_country == "Netherlands" ~ 0,
                                        origin_country == "Greece" ~ 0,
                                        origin_country == "Portugal" ~ 0,
                                        origin_country == "Ireland" ~ 0,
                                        origin_country == "Austria" ~ 0,
                                        origin_country == "Hungary" ~ 1, # 1 = post-2004 EU members
                                        origin_country == "Poland" ~ 1,
                                        origin_country == "Czechrepublic" ~ 1,
                                        origin_country == "Slovakia" ~ 1,
                                        origin_country == "UK" ~ 2, # 2 = not EU members as of 2021
                                        origin_country == "US" ~ 2,
                                        origin_country == "Norway" ~ 2,
                                        origin_country == "Switzerland" ~ 2,
                                        TRUE ~ 0),
         origin_country_character = case_when(origin_country_new == 0 ~ "Old",
                                              origin_country_new == 1 ~ "New",
                                              origin_country_new == 2 ~ "Non"))


sdata$origin_country_character <- factor(sdata$origin_country_character, levels = c("Old","New","Non")) # setting "old" as reference category.

factor(sdata$origin_country_character)

# Changing the order so that business is reference-category in the numeric category variable:
sdata$category_factor <- factor(sdata$category_numeric, levels = c(2,1,3,4,5,6)) 

