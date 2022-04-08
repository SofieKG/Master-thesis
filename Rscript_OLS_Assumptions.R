### OLS Assumptions ### 

M <- lm(success ~ fte + sal_value + coal_value + origin_country_new + 
          brussels_office + Category_factor, data=sdata) # creating a linear model to use in the tests.

# Linearity:

ceresPlot(M, variable = "fte")# linear
ceresPlot(M, variable = "sal_value") # not linear, not enough observations, I'll turn it into a dichotomous variable.

unique(sdata$sal_value)
(77 + 73 + 42 + 63 + 49 + 19 + 39 + 42)/8

# Find that the average salience value = 50.5. All issues with sal_value >= 50.5 = 1,
# issues <= 50.5 = 0.

sdata <- sdata %>% 
  mutate(new_salience = ifelse(sal_value <= 50.5, 0, 1)) 

rm(M)

M1 <- lm(success ~ fte + new_salience + fte*new_salience + coal_value + origin_country_new + brussels_office + Category_factor, data=sdata)  

# the final three independent variables are dichotomous, and do not need this test. 


# Autocorrelation:

# robust, clustered standard errors: ??? 

robust_model <- lm(success ~ fte + new_salience + 
                     fte*new_salience + coal_value + 
                     origin_country_new +
                     brussels_office + category_factor,
                   data = sdata)

coeftest(robust_model, vcov = vcovCL(robust_model, cluster = ~category))

summary(robust_model)

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
