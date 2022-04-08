### ANALYSIS ### 

## Pre-regression variable checks ##

descriptive_statistics <- 
  data.frame(sdata[, c("success", "category_numeric", "sal_value", "fte",
                       "brussels_office", "coal_value", "origin_country_new", "new_salience")])

stargazer(descriptive_statistics,
          type = "text",
          out = "overview_variables.html",
          title = "Descriptive Statistics",
          digits = 2,
          covariate.labels = c("Success", "Category numeric", "Salience", "Full time equivalent", 
                               "Office in Brussels", "Coalition", "Origin Country New", "Dichotomous Salience"),
          summary.stat = c("n", "min", "max", "mean", "median", "sd"),
          font.size = ("footnotesize"))

## Hypothesis 1 ##
# Have interest groups representing business had a higher level of success compared to other categories?

# Find the mean of success for each of the interest group categories, and including the standard deviation: 
success_by_category <- sdata %>% 
  group_by(category_numeric) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(category_numeric))

# or

success_by_category_name <- sdata %>% 
  group_by(category) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(category))


# Doing the same, but comparing business with all other categories (combining all other categories): 
success_by_category2 <- sdata %>% 
  mutate(new_category = ifelse(category_numeric == 2, "Business Associations", "All Others")) %>% 
  group_by(new_category) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(new_category))


# Comparing NGOs with all other categories combined:
success_by_category3 <- sdata %>% 
  mutate(new_category = ifelse(category_numeric == 1, "Non-governmental Organisations", "Commercial Interests")) %>% 
  group_by(new_category) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(new_category))


## Regression H1 ##


H1 <- lm(success ~ category_factor, data = sdata) # with no controls

H1_controlled <- lm(success ~ category_factor + new_salience + 
                    fte + coal_value + origin_country_new + brussels_office, 
                    data = sdata) # with controls


stargazer(H1, H1_controlled, title = "H1 Results", 
          dep.var.labels = "Interest group success", 
          covariate.labels = c ("NGOs", "Groups and companies", 
                    "Trade unions and professional associations",
                    "Other organisations", "Think tanks and research institutions",
                    "Salience", "Full time equivalent", "Coalition",
                    "Origin Country", "Brussels Office"),
          align = TRUE, type = "text", out = "Model 1.html")


## Regression H2 ## 

# H2 Resource-hypothesis: The more employees (ftes) an interest group has working specifically 
# on lobbying in the EU, the more likely it is to reach its preference attainment. 


H2 <- lm(success ~ fte, data = sdata) # with no controls

stargazer(H2, title = "H2 Results",  
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("Full Time Equivalent"),
          align = TRUE, type = "text", out = "Model 2.html") 



## H2 WITHOUT AND WITH CONTROLS ##

H2_controls <- lm(success ~ fte + coal_value + origin_country_new + 
                    brussels_office + category_factor, data = sdata) 


stargazer(H2, H2_controls, title = "H2 Results controlled", 
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("Full Time Equivalent", "Coalition", 
                                "Origin Country", "Brussels Office",
                                "NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions"),
          align = TRUE, type = "text",  out = "Model 3.html")


## Regression H3 ## 

# H3 Salience-hypothesis: When salience is high, resources become 
# less important when an interest group attempts to reach their preferred outcome. 

H3 <- lm(success ~ fte + new_salience + fte*new_salience, data = sdata) # with no controls

stargazer(H3, title = "H3 Results", 
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("Full Time Equivalent", "Salience", 
                                "Fte*Salience Interaction"),
          align = TRUE, type = "text", out = "Model 4.html")


## H3 WITHOUT AND WITH CONTROLS ##

H3_controls <- lm(success ~ fte + new_salience + fte*new_salience + 
                    coal_value + origin_country_new + brussels_office + 
                    category_factor, data = sdata) 


stargazer(H3, H3_controls, title = "H3 Results controlled", 
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("Full Time Equivalent", "Salience", 
                                "Coalition", 
                                "Origin Country", "Brussels Office",
                                "NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Fte*Salience Interaction"),
          align = TRUE, type = "text",  out = "Model 5.html") 
