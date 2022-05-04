### OLS Assumptions ### 

M <- lm(success ~ fte + sal_value + coal_value + origin_country_character +
          brussels_office + category_factor, data=sdata) # creating a linear model to use in the tests. 
# Use origin_country_new here, as the new variable is a character-one.

# Linearity:

ceresPlot(M, variable = "fte")# linear
ceresPlot(M, variable = "sal_value") # not linear, not enough observations, I'll turn it into a dichotomous variable.

unique(sdata$sal_value)
(77 + 73 + 42 + 63 + 49 + 19 + 39 + 42)/8

# Find that the average salience value = 50.5. All issues with sal_value >= 50.5 = 1,
# issues < 50.5 = 0.

sdata <- sdata %>% 
  mutate(new_salience = ifelse(sal_value < 50.5, 0, 1)) 

rm(M)

M1 <- lm(success ~ fte + new_salience + fte*new_salience + coal_value + origin_country_character + brussels_office + category_factor, data=sdata)  

# the final three independent variables are dichotomous, and do not need this test. 


# Autocorrelation:

# robust, clustered standard errors:  

robust_model <- lm(success ~ fte + new_salience + 
                     fte*new_salience + coal_value + 
                     origin_country_character +
                     brussels_office + category_factor,
                   data = sdata)

robust_model2 <- coeftest(robust_model, vcov = vcovCL(robust_model, cluster = ~groupname))

# Create a stargazer-table in order to visualise the difference:
stargazer(M1, robust_model2, title = "Clustered Standard Errors",
          covariate.labels = c ("Full Time Equivalent", "Salience",
                                "Coalition", 
                                "New Member State", " Non-EU State","Brussels Office",
                                "NGOs", "Groups and Companies", 
                                "Trade Unions and Professional Associations",
                                "Other Organisations", 
                                "Think Tanks and Research Institutions",
                                "Fte*Salience Interaction"),
          align = TRUE, type = "text",  
          notes = ("Ref.cat Categories = Trade and Business Associations, Ref.cat Origin Country = Old"),
          out = ("Clustered Std.Errors.html")) 

# Zero conditional mean:

ggplot() +
  geom_histogram(aes(x = rstandard(M1),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2")


# A definite peak near the middle, with two accompanying smaller peaks at each side of the histogram.

# Homoscedasticity:

residualPlot(M1)

# Lots of values on 20 and 40, but great spread given that it is only two cases. 


# Multicollinearity:

vif(M1)

# Discussion in thesis.


## Skewness + kurtosis ##

# I do not check the dummy-variables.

kurtosis((sdata$success), na.rm = T) # 2.35
kurtosis((sdata$new_salience), na.rm = T) # 1.30
kurtosis((sdata$fte), na.rm = T) # 11.6
kurtosis((sdata$coal_value), na.rm = T) # 3.71
kurtosis((sdata$brussels_office), na.rm = T) # 1.38


skewness(sdata$success, na.rm = T) # 2.16
skewness(sdata$new_salience, na.rm = T) # -0.55
skewness(sdata$fte, na.rm = T) # 2.85
skewness(sdata$coal_value, na.rm = T) # -1.64
skewness(sdata$brussels_office, na.rm = T) # -0.62
