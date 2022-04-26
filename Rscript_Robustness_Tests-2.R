### Robustness tests ###

install.packages("plm")
library(plm)

## ROBUSTNESS TEST 1: AUTOCORRELATION, CAN BE FOUND IN Rscript_OLS_Assmuptions.R ##

## ROBUSTNESS TEST 2: FIXED EFFECTS ##

## Fixed-effects at the issue-level:
plm_fe_issue <- plm(data = sdata, 
                  success ~ category_factor + fte + 
                    new_salience + brussels_office + 
                    coal_value + origin_country_character + 
                    fte*new_salience,
                  na.action = "na.exclude", model = "within", index = "issue", effect = "individual")


summary(plm_fe_issue)

## Fixed-effects at the case-level

# First I need to sort the issues into their respective cases:

sdata <- sdata %>%
  mutate(case = ifelse(issue == "issue1" | 
                         issue == "issue2" |
                         issue == "issue3" |
                         issue == "issue 4" |
                         issue == "issue 5",
                        "Case 1", "Case 2"))
         

plm_fe_case <- plm(data = sdata, 
                    success ~ category_factor + fte + 
                      new_salience + brussels_office + 
                      coal_value + origin_country_character + 
                      fte*new_salience,
                    na.action = "na.exclude", model = "within", index = "case", effect = "individual")


summary(plm_fe_case)


# Running my Model 5 from the analysis again in order to be able to compare results:

H_ALL_CONTROLLED <- lm(success ~ category_factor + fte + 
                         new_salience + brussels_office + 
                         coal_value + origin_country_character + 
                         fte*new_salience,
                       data = sdata)


# Creating a table where I compare the FE with the ordinary OLS:

stargazer(H_ALL_CONTROLLED, plm_fe_case, plm_fe_issue, type = "text",
          column.labels = c("No FE","FE Case", "FE Issue"),
          covariate.labels = c ("NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Full Time Equivalent", "Salience",
                                "Brussels Office",
                                "Coalition", 
                                "New Member State", " Non-EU State",
                                "Fte*Salience Interaction"),
          notes = ("Ref.cat Categories = Trade and Business Associations, Ref.cat Origin Country = Old"),
          out = "Fixed Effects.html") 


## ROBUSTNESS TEST 3: LOGISTICAL REGRESSION WITH REDUCED SAMPLE ##
# Drawing out those that had min (0) and max (60) score on the dependent variable: 

summary(sdata$success)

new_sdata <- subset(sdata, success>0 & success<60) # creating a new data frame without these rows

Regression_reduced_sample <- lm(success ~ category_factor + fte + 
                         new_salience + brussels_office + 
                         coal_value + origin_country_character + 
                         fte*new_salience,
                         data = new_sdata)
  
stargazer(H_ALL_CONTROLLED, Regression_reduced_sample, title = "Reduced Sample Regression", 
          column.labels = c("Ordinary Model","Reduced Sample"),
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Full Time Equivalent", "Salience",
                                "Brussels Office",
                                "Coalition", 
                                "New Member State", " Non-EU State",
                                "Fte*Salience Interaction"),
          align = TRUE, type = "text",  
          notes = ("Ref.cat Categories = Trade and Business Associations, Ref.cat Origin Country = Old"),
          out = "Reduced Sample Regression.html")


## ROBUSTNESS TEST 4: LOGIT-MODEL ##

# First off, I transform the dependent variable (success) into a binomial one: 

sdata <- sdata %>% 
  mutate(b_success = ifelse(success < 30, 0, 1)) # >30 = 0, 30-60 = 1

# Running a logistic regression using glm: 

logistic_regression <- glm(b_success ~ category_factor + fte + 
                         new_salience + brussels_office + 
                         coal_value + origin_country_character + 
                         fte*new_salience,
                         family = "binomial",
                         na.action = "na.exclude",
                       data = sdata)
  

summary(logistic_regression)

# Plotting the results from the logistic regression next to Model 5 from the analysis:

stargazer(H_ALL_CONTROLLED, logistic_regression, title = "Logistic Regression", 
          column.labels = c("Ordinary Model","Logistic Regression"),
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Full Time Equivalent", "Salience",
                                "Brussels Office",
                                "Coalition", 
                                "New Member State", " Non-EU State",
                                "Fte*Salience Interaction"),
          align = TRUE, type = "text",  
          notes = ("Ref.cat Categories = Trade and Business Associations, Ref.cat Origin Country = Old"),
          out = "Logistic Regression.html")


