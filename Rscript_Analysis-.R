### ANALYSIS ### 

## Pre-regression variable checks ##

# Descriptive statistics of the numeric variables:

descriptive_statistics <- 
  data.frame(sdata[, c("success", "fte", "new_salience",
                       "brussels_office", "coal_value")]) 

stargazer(descriptive_statistics,
          type = "text",
          out = "overview_variables.html",
          title = "Descriptive Statistics",
          digits = 2,
          covariate.labels = c("Success", "Full time equivalent", 
                               "Dichotomous Salience", "Office in Brussels", 
                               "Coalition"),
          summary.stat = c("n", "min", "max", "mean", "median", "sd"),
          font.size = ("footnotesize"))


# Descriptive statistic of the categorical variables:

# Plotting the category-variable:

ggplot(data=sdata, aes(as.factor(category_numeric)), theme_bw()) +
  geom_bar() +
  theme_bw() +
  scale_y_discrete("Observations") +
scale_x_discrete("Category", labels = c("1" = "NGO", "2" = "Trade & Business Groups",
                              "3" = "Groups & Companies", 
                              "4" = "Trade Uni. & Prof.Associations",
                              "5" = "Other", "6" = "Think Tanks & Research"))


# Plotting the origin_country-variable:

ggplot(data=sdata, aes(as.factor(origin_country_new)), theme_bw()) +
  geom_bar() +
  theme_bw() +
  scale_y_discrete("Observations") +
  scale_x_discrete("Country of Origin", labels = c("0" = "Member pre-2004", "1" = "Member in or post-2004",
                                          "2" = "Non-EU Member")) 



## Hypothesis 1 ##
# Have interest groups representing business had a higher level of success compared to other categories?

# Find the mean of success for each of the interest group categories, and including the standard deviation: 
success_by_category <- sdata %>% 
  group_by(category_numeric) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(category_numeric))


# Comparing all public interests with all commercial categories combined:

success_by_category3 <- sdata %>% 
  mutate(new_category = ifelse(category_numeric == 1 | 
                                 category_numeric == 6,
                               "Public Interests", "Other"))  %>% 
  group_by(new_category) %>% 
  summarise(success_category=mean(success), success_sd = sd(success), n = length(new_category)) # To find the score for the public interests.

success_by_category4 <- sdata %>% 
  mutate(new_category = ifelse(category_numeric == 2 | 
                                 category_numeric == 3,
                               "Commercial Interests", "Other"))  %>% 
  group_by(new_category) %>% 
  summarise(success_category=mean(success), success_sd = sd(success), n = length(new_category)) # To find the score for the commercial interests.


## Regression H1 ##

# H1 Business success-hypothesis: those representing commercial interests have been less successful than
# public interests in influencing the EC in recent climate action policy processes.


H1 <- lm(success ~ category_factor, data = sdata) # H1 with no controls

H1_controlled <- lm(success ~ category_factor + brussels_office + 
                      coal_value + origin_country_character, data = sdata) # H1 with controls


# Running an t-test to check the results, as I have a categorical independent and continous dependent variable:

sdata$cat <- ifelse(sdata$category_numeric==1, "Pub", ifelse(sdata$category_numeric==6, "Pub", 
             ifelse(sdata$category_numeric==2, "Comm", ifelse(sdata$category_numeric==3, "Comm", 
             ifelse(sdata$category_numeric==4, "Other", ifelse(sdata$category_numeric==5, "Other",NA)))))) # Creating variable sorting the categories.


sdata$comm <- ifelse(sdata$cat == "Comm", 1, ifelse(sdata$cat == "Pub", 0, NA)) # Creating dummy for commercial interests, removing the category "Other" so I can compare public and commercial interests throughout the analysis.
sdata$public <- ifelse(sdata$cat == "Pub", 1, ifelse(sdata$cat == "Comm", 0, NA)) # Dummy for public interests.


# Performing a t-test to see whether there is a significant difference between the two groups:

t.test(success ~ comm, data = sdata, alternative = "two.sided", # Using the variable "comm" where 0 = public and 1 = commercial interest as the predictor.
       var.equal = TRUE)


## Regression H2 ## 

# H2 Resource-hypothesis: The more employees (fte's) an interest group has working specifically 
# on lobbying in the EU, the more likely it is to reach its preference attainment. 

H2 <- lm(success ~ fte, data = sdata) # H2 with no controls

H2_controlled <- lm(success ~ fte + brussels_office + 
                      coal_value + origin_country_character, data = sdata) # H2 with controls


# which group has on average the most fte's?:

fte_by_category <- sdata %>% 
  group_by(category) %>% 
  summarise(fte_category_mean=mean(fte), fte_sd = sd(fte), n = length(category))


# Performing a t-test to see whether there is a significant difference in the two groups:

t.test(fte ~ comm, data = sdata, alternative = "two.sided", # Using the variable "comm" where 0 = public and 1 = commercial interest as the predictor.
       var.equal = TRUE)


## Regression H3 ## 
# H3 Salience-hypothesis: When salience is high average success for all is moderated:

H3 <- lm(success ~ new_salience, data = sdata) # H3 without controls

H3_controlled <- lm(success ~ new_salience + brussels_office + 
                      coal_value + origin_country_character, data = sdata) # H3 with controls


# are public interests more successful in highly salient issues?

h_sal_high <- sdata[sdata$new_salience == 1, ] # creating a dataframe with only those rows where salience is high

success_by_category_name <- h_sal_high %>% # to check the success of the different groups on these issues.
  group_by(category) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(category))


# Performing a t-test to see whether there is a significant difference in the two groups:

t.test(success ~ comm, data = h_sal_high, alternative = "two.sided", # Using the variable "comm" where 0 = public and 1 = commercial interest as the predictor.
       var.equal = TRUE)



h_sal_low <- sdata[sdata$new_salience == 0, ]

success_by_category_name_l <- h_sal_low %>% # to check the success of the different groups on issues of low salience to compare.
  group_by(category) %>% 
  summarise(success_category=mean(success), sucess_sd = sd(success), n = length(category))


# Performing a t-test to see whether there is a significant difference in the two groups:

t.test(success ~ comm, data = h_sal_low, alternative = "two.sided", # Using the variable "comm" where 0 = public and 1 = commercial interest as the predictor.
       var.equal = TRUE)



## Regression H4 ## 

# H4 Salience interaction-hypothesis: When salience is high, resources become 
# less important when an interest group attempts to reach their preferred outcome. 

H4 <- lm(success ~ fte + new_salience + fte*new_salience, data = sdata) # with no controls

H4_controlled <- lm(success ~ fte + new_salience + fte*new_salience + brussels_office + 
                      coal_value + origin_country_character, data = sdata) # H4 with controls


# Plotting the interaction-effect using sjPlot:

library(sjPlot)

fit <- lm(success ~ fte + fte*new_salience, data = sdata)

plot_model(fit, type = "pred", terms = c("fte", "new_salience"), title = "Predicted values of success based on fte*salience",
           axis.title = "Success", scale_color_discrete(labels = c("Low salience", "high salience")))



## Running a regression with all explanatory + control variables:

H_ALL_CONTROLLED <- lm(success ~ category_factor + fte + 
                         new_salience + brussels_office + 
                         coal_value + origin_country_character + 
                         fte*new_salience,
                       data = sdata)



### Visualising the models: 

stargazer(H1, H1_controlled, 
          title = "Hypothesis 1", 
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Brussels Office",
                                "Coalition", 
                                "New Member State", " Non-EU State"),
          align = TRUE, type = "text",  
          notes = ("Ref.cat Categories = Trade and Business Associations"),
          out = "H1.html")


stargazer(H2, H2_controlled, H3, H3_controlled, title = "Hypothesis 2 and 3", 
          dep.var.labels = "Interest Group Success", 
          covariate.labels = c ("Full Time Equivalent", 
                                "Brussels Office",
                                "Coalition", 
                                "New Member State", " Non-EU State",
                                "Salience"),
          align = TRUE, type = "text",  
          notes = ("Ref.cat Origin Country = Old"),
          out = "H2 + H3.html")



stargazer(H4, H4_controlled, H_ALL_CONTROLLED, title = "Hypothesis 4 and Full model", 
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
          out = "H4 + full model.html")



# Visualising the coefficients and their confidence intervals:

install.packages("dotwhisker") # In order to visualise the coefficients differently, I decided to run a dwplot.
library(dotwhisker)

dwplot(H_ALL_CONTROLLED, dot_args = list(size = 3, pch = 21, fill = "red")) + # with 95% confidence intervals
    xlab("Coefficient") +
  ggtitle("OLS estimates of all independent variables on Success") +
  labs(caption = "Ref.cat category = Trade and Business groups, Ref.cat origin country = Old EU MS") +
       scale_y_discrete(labels = c("category_factor1" = "NGOs", 
                                               "category_factor3" = "Groups & Companies", 
                                               "category_factor4" = "Trade Uni. & Prof.Associations",
                                               "category_factor5" = "Other Organisation", "category_factor6" = "Think Tanks & Research",
                                               "fte" = "Full-Time Equivalent", "new_salience" = "Salience",
                                               "brussels_office" = "Brussels Office",
                                   "coal_value" = "Coalition", "origin_country_characterNew" = "Post-2004 Member",
                                   "origin_country_characterNon" = "Non EU Member", "fte:new_salience" = "Fte*Salience Interaction"))


dwplot(H_ALL_CONTROLLED, ci = 0.80, dot_args = list(size = 3, pch = 21, fill = "red")) + # with 80% confidence intervals
  xlab("Coefficient") +
  ggtitle("OLS estimates of all independent variables on Success") +
  labs(caption = "Ref.cat category factor = Trade and Business groups, Ref.cat origin_country_character = Old EU MS") 


## Plotting independent variables:
# Plotting the explanatory variables:

ggplot(sdata, aes(category_factor, success, colour = category_factor)) + # plotting success of each category
  geom_count() +
labs(title = "Success of each category",
     x = "Category interest groups",
     y = "Success",
     caption = "
     2 = Trade and Business Groups, 
     1 = NGOs, 
     3 = Groups and Companies, 
     4 = Trade Unions and Professional Associations,
     5 = Other Organisations, 
     6 = Think Tanks and Research Institutions")


a <- ggplot(sdata, aes(x = fte, y = success)) # plotting fte on success.

a + geom_jitter() +
geom_smooth(method = "loess") +
  labs(x = "Full Time Equivalents", y = "Success",
       title = "Success depending on Fte")

ggplot(sdata, aes(y = success, x = new_salience)) +
  geom_smooth(method = "lm") +
  labs( x = "Dichotmous Salience", y = "Success",
        title = "Success dependent on Salience",
        caption = "Low salience = 0, High salience = 1")

ggplot(sdata, aes(y = success, x = fte, color = as.factor(new_salience))) + # adding salience for the interaction effect
  geom_jitter(alpha = 1) +
  labs(x = "Full Time Equivalents", y = "Success",
       title = "Success by Fte, dependent on Salience",
       caption = "Low salience = 0, High salience = 1")


# Plotting the control variables:

ggplot(sdata, aes(coal_value, success)) + # plotting the effect of coalition on success
  geom_smooth(method = "lm") +
  labs(x = "Coalition", y = "Success",
     title = "Success by Coalition",
     caption = "Minority = 0, Majority = 1")


ggplot(sdata, aes(brussels_office, success)) + # plotting the effect of Brussels office on success. 
  geom_smooth(method = "lm") +
  labs(x = "Brussels Office", y = "Success",
       title = "Success by location of EU affairs",
       caption = "No Brussels office = 0, Brussels office = 1")


ggplot(sdata, aes(origin_country_character, success)) + # plotting how origin country affects success.
  geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2, color = "tomato") +
  labs(x = "Origin Country", y = "Success",
       title = "Success by Origin Country")

         