### ANALYSIS ### 

## Pre-regression variable checks ##

descriptive_statistics <- 
  data.frame(sdata[, c("success", "category_factor", "fte", "new_salience",
                       "origin_country_new",
                       "brussels_office", "coal_value")]) 

stargazer(descriptive_statistics,
          type = "text",
          out = "overview_variables.html",
          title = "Descriptive Statistics",
          digits = 2,
          covariate.labels = c("Success", "Category", "Full time equivalent", 
                               "Dichotomous Salience",
                               "Origin Country","Office in Brussels", 
                               "Coalition"),
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


# Comparing all public interests with all commercial categories combined:
success_by_category3 <- sdata %>% 
  mutate(new_category = ifelse(category_numeric == 1 | 
                                 category_numeric == 6 |
                                 category_numeric == 5, 
                               "Public Interests", "Commercial Interests"))  %>% 
  group_by(new_category) %>% 
  summarise(success_category=mean(success), success_sd = sd(success), n = length(new_category))


## Regression H1 ##


H1 <- lm(success ~ category_factor, data = sdata) # with no controls


## Regression H2 ## 

# H2 Resource-hypothesis: The more employees (fte's) an interest group has working specifically 
# on lobbying in the EU, the more likely it is to reach its preference attainment. 

H2 <- lm(success ~ fte, data = sdata) # with no controls


# which group has on average the most fte's?:

fte_by_category <- sdata %>% 
  group_by(category) %>% 
  summarise(fte_category_mean=mean(fte), fte_sd = sd(fte), n = length(category))


## Regression H3 ## 

# H3 Salience-hypothesis: When salience is high, resources become 
# less important when an interest group attempts to reach their preferred outcome. 

H3 <- lm(success ~ fte + new_salience + fte*new_salience, data = sdata) # with no controls

# Plotting the interaction-effect between fte and salience:

predframe1 <- tibble(fte = seq(.2,41.8,.1), new_salience = 0)

prediction1 <- tibble(prediction = predict(H3, predframe1), fte = seq(.2,41.8,.1)) %>%
  mutate(salience = "low")

predframe2 <- tibble(fte = seq(.2,41.8,.1), new_salience = 1)

prediction2 <- tibble(prediction = predict(H3, predframe2), fte = seq(.2,41.8,.1)) %>%
  mutate(salience = "high")

predfull <- rbind(prediction1, prediction2) # binding the two frames

unique(predfull$salience)


# Plotting the interaction-effect using ggplot:

predfull %>%
  ggplot(aes(x=fte, y=prediction, color = salience)) +
  geom_line() +
  labs(title = "Interaction visualised",
       x = "Full Time Equivalent",
       y = "Prediction",
       caption = "High salience = 1, 
       Low salience = 0")


## Running a regression with all explanatory variables:

H_ALL <- lm(success ~ category_factor + fte + new_salience + fte*new_salience, data = sdata) # with no controls
  

## Running a regression with all explanatory + control variables:

H_ALL_CONTROLLED <- lm(success ~ category_factor + fte + 
                         new_salience + brussels_office + 
                         coal_value + origin_country_character + 
                         fte*new_salience,
                         data = sdata)
  

### All models visualised in a table: 

stargazer(H1, H2, H3, H_ALL, H_ALL_CONTROLLED, title = "All Results Controlled", 
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
          out = "All Models.html")


## Plotting results:
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


ggplot(sdata, aes(y = success, x = fte, color = as.factor(new_salience))) + # adding salience
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

         